module Main exposing (main)

import Browser
import Generated.API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Items
import Tags


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type Model
    = Loading
        { items : Maybe (List ItemBrief)
        , tags : Maybe (List Tag)
        }
    | LoadingFailed String
    | Model
        { items : Items.Model
        , tags : Tags.Model
        }


withResult : (e -> r) -> (a -> r) -> Result e a -> r
withResult onErr onOk result =
    case result of
        Err err ->
            onErr err

        Ok ok ->
            onOk ok


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
        { items = Nothing
        , tags = Nothing
        }
    , Cmd.batch
        [ getApiItems (withResult GotLoadingError LoadedItemList)
        , getApiTags (withResult GotLoadingError LoadedTagList)
        ]
    )


type Msg
    = LoadedItemList (List ItemBrief)
    | LoadedTagList (List Tag)
    | GotLoadingError Http.Error
    | TagMsg Tags.Msg
    | ItemMsg Items.Msg


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Invalid url: " ++ url

        Http.Timeout ->
            "Timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "Unexpected HTTP status code: " ++ String.fromInt code

        Http.BadBody decErr ->
            decErr


loadedState : { items : Maybe (List ItemBrief), tags : Maybe (List Tag) } -> ( Model, Cmd Msg )
loadedState load =
    case ( load.items, load.tags ) of
        ( Just items, Just tags ) ->
            let
                ( itemsModel, itemsCmd ) =
                    Items.init items

                tagsModel =
                    Tags.init tags
            in
            ( Model { items = itemsModel, tags = tagsModel }
            , Cmd.map ItemMsg itemsCmd
            )

        ( items, tags ) ->
            ( Loading { items = items, tags = tags }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotLoadingError err, _ ) ->
            ( LoadingFailed (httpErrorToString err), Cmd.none )

        ( LoadedItemList items, Loading load ) ->
            loadedState { load | items = Just items }

        ( LoadedItemList _, _ ) ->
            ( model, Cmd.none )

        ( LoadedTagList tags, Loading load ) ->
            loadedState { load | tags = Just tags }

        ( LoadedTagList _, _ ) ->
            ( model, Cmd.none )

        ( ItemMsg itemMsg, Model mod ) ->
            let
                ( newItems, cmd ) =
                    Items.update itemMsg mod.items
            in
            ( Model { mod | items = newItems }, Cmd.map ItemMsg cmd )

        ( ItemMsg _, _ ) ->
            ( model, Cmd.none )

        ( TagMsg tagMsg, Model mod ) ->
            let
                ( newTags, cmd ) =
                    Tags.update tagMsg mod.tags
            in
            ( Model { mod | tags = newTags }, Cmd.map TagMsg cmd )

        ( TagMsg _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            text "Loading..."

        LoadingFailed err ->
            span [ style "color" "red" ] [ text err ]

        Model { items, tags } ->
            div []
                [ Html.map ItemMsg <|
                    div [] <|
                        Items.view (Tags.tagsFromModel tags) items
                , Html.map TagMsg <|
                    div [] <|
                        Tags.view tags
                ]
