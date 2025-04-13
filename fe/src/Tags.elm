module Tags exposing
    ( Model
    , Msg(..)
    , formatColor
    , init
    , tagsFromModel
    , update
    , view
    )

import Generated.API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style, type_, value)
import Html.Events exposing (..)
import List.Extra exposing (..)
import Result.Extra exposing (..)


type alias Model =
    List TagModel


type alias Nonce =
    Int


type TagModel
    = Normal
        { tag : Tag
        , editing : Maybe EditingData
        }
    | New
        { nonce : Nonce
        , editing : EditingData
        }


type alias EditingData =
    { inputName : String
    , name : Maybe String
    , inputColor : String
    , color : Maybe Color
    }


validateName : String -> Maybe String
validateName str =
    if String.isEmpty str then
        Nothing

    else
        Just str


validateColor : String -> Maybe Color
validateColor str =
    if not (String.startsWith "#" str) then
        Nothing

    else if String.length str /= 7 then
        Nothing

    else if not (String.all Char.isHexDigit (String.dropLeft 1 str)) then
        Nothing

    else
        Just (String.toLower (String.dropLeft 1 str))


formatColor : Color -> String
formatColor color =
    "#" ++ color


init : List Tag -> Model
init =
    List.map (\tag -> Normal { tag = tag, editing = Nothing })


tagsFromModel : Model -> List Tag
tagsFromModel =
    List.filterMap
        (\mod ->
            case mod of
                Normal { tag } ->
                    Just tag

                New _ ->
                    Nothing
        )


type Msg
    = NoOp
    | EditClicked TagId
    | EditUpdated TagId EditingData
    | EditSubmitted TagId TagInsert
    | Edited Tag
    | EditCancelled TagId
    | DeleteClicked TagId
    | Deleted TagId
    | NewClicked
    | NewUpdated Nonce EditingData
    | NewSubmitted Nonce TagInsert
    | NewCreated Nonce Tag
    | NewCanceled Nonce


findUnusedNonce : Model -> Nonce
findUnusedNonce =
    List.foldl
        (\mod n ->
            case mod of
                New { nonce } ->
                    max (nonce + 1) n

                _ ->
                    n
        )
        0


newEditing : EditingData
newEditing =
    let
        inputName =
            ""

        inputColor =
            "#ff0000"
    in
    { inputName = inputName
    , name = validateName inputName
    , inputColor = inputColor
    , color = validateColor inputColor
    }


updateNormalTag : List TagModel -> TagId -> (Tag -> Maybe EditingData -> TagModel) -> List TagModel
updateNormalTag tags id f =
    tags
        |> updateFirstMatch
            (\mod ->
                case mod of
                    Normal { tag, editing } ->
                        if tag.id == id then
                            Just (f tag editing)

                        else
                            Nothing

                    _ ->
                        Nothing
            )


updateNewTag : List TagModel -> Nonce -> (EditingData -> TagModel) -> List TagModel
updateNewTag tags n f =
    tags
        |> updateFirstMatch
            (\mod ->
                case mod of
                    New { nonce, editing } ->
                        if nonce == n then
                            Just (f editing)

                        else
                            Nothing

                    _ ->
                        Nothing
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg tags =
    case msg of
        NoOp ->
            ( tags, Cmd.none )

        EditClicked id ->
            ( updateNormalTag tags id <|
                \tag _ ->
                    Normal
                        { tag = tag
                        , editing =
                            Just
                                { inputName = tag.name
                                , name = Just tag.name
                                , inputColor = formatColor tag.color
                                , color = Just tag.color
                                }
                        }
            , Cmd.none
            )

        EditUpdated id editing ->
            ( updateNormalTag tags id <|
                \tag _ -> Normal { tag = tag, editing = Just editing }
            , Cmd.none
            )

        EditSubmitted id ins ->
            ( tags
            , putApiTagsByTag_id id ins (withResult (\_ -> NoOp) Edited)
            )

        Edited tag ->
            ( updateNormalTag tags tag.id <|
                \_ _ -> Normal { tag = tag, editing = Nothing }
            , Cmd.none
            )

        EditCancelled id ->
            ( updateNormalTag tags id <|
                \tag _ -> Normal { tag = tag, editing = Nothing }
            , Cmd.none
            )

        DeleteClicked id ->
            ( tags
            , deleteApiTagsByTag_id id (withResult (\_ -> NoOp) (\_ -> Deleted id))
            )

        Deleted id ->
            ( tags
                |> deleteFirstMatch
                    (\mod ->
                        case mod of
                            Normal { tag } ->
                                tag.id == id

                            _ ->
                                False
                    )
            , Cmd.none
            )

        NewClicked ->
            ( tags ++ [ New { nonce = findUnusedNonce tags, editing = newEditing } ]
            , Cmd.none
            )

        NewUpdated nonce editing ->
            ( updateNewTag tags nonce <|
                \_ -> New { nonce = nonce, editing = editing }
            , Cmd.none
            )

        NewSubmitted nonce ins ->
            ( tags
            , postApiTags ins (withResult (\_ -> NoOp) (NewCreated nonce))
            )

        NewCreated nonce tag ->
            ( updateNewTag tags nonce <|
                \_ -> Normal { tag = tag, editing = Nothing }
            , Cmd.none
            )

        NewCanceled nonce ->
            ( tags
                |> deleteFirstMatch
                    (\mod ->
                        case mod of
                            New tag ->
                                tag.nonce == nonce

                            _ ->
                                False
                    )
            , Cmd.none
            )


editingColorStyle : EditingData -> List (Attribute msg)
editingColorStyle edit =
    case edit.color of
        Just color ->
            [ style "color" (formatColor color) ]

        Nothing ->
            []


editNameView : EditingData -> (EditingData -> msg) -> Html msg
editNameView edit onEdit =
    input
        (editingColorStyle edit
            ++ [ type_ "text"
               , onInput
                    (onEdit
                        << (\name -> { edit | inputName = name, name = validateName name })
                    )
               , value edit.inputName
               ]
        )
        []


editColorView : EditingData -> (EditingData -> msg) -> Html msg
editColorView edit onEdit =
    input
        [ type_ "color"
        , onInput
            (onEdit
                << (\color -> { edit | inputColor = color, color = validateColor color })
            )
        , value edit.inputColor
        ]
        []


submitView : EditingData -> (TagInsert -> msg) -> Html msg
submitView edit onSubmit =
    button
        (case ( edit.name, edit.color ) of
            ( Just name, Just color ) ->
                [ onClick (onSubmit { name = name, color = color }) ]

            _ ->
                [ disabled True ]
        )
        [ i [ class "fa fa-save" ] [] ]


view : Model -> List (Html Msg)
view tags =
    h2 [] [ text "Tags" ]
        :: List.map
            (\mod ->
                case mod of
                    Normal { tag, editing } ->
                        case editing of
                            Nothing ->
                                span
                                    [ style "color" (formatColor tag.color) ]
                                    [ text tag.name
                                    , button [ onClick (EditClicked tag.id) ] [ i [ class "fa fa-edit" ] [] ]
                                    ]

                            Just edit ->
                                span []
                                    [ editNameView edit (EditUpdated tag.id)
                                    , editColorView edit (EditUpdated tag.id)
                                    , submitView edit (EditSubmitted tag.id)
                                    , button [ onClick (EditCancelled tag.id) ] [ i [ class "fa fa-times-circle" ] [] ]
                                    , button [ onClick (DeleteClicked tag.id) ] [ i [ class "fa fa-trash-alt" ] [] ]
                                    ]

                    New { nonce, editing } ->
                        span []
                            [ editNameView editing (NewUpdated nonce)
                            , editColorView editing (NewUpdated nonce)
                            , submitView editing (NewSubmitted nonce)
                            , button [ onClick (NewCanceled nonce) ] [ i [ class "fa fa-times-circle" ] [] ]
                            ]
            )
            tags
        ++ [ button [ onClick NewClicked ] [ i [ class "fa fa-plus" ] [] ] ]
