module Items exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Generated.API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, disabled, selected, style, type_, value)
import Html.Events exposing (..)
import Html5.DragDrop
import List.Extra exposing (..)
import Result.Extra exposing (..)
import Tags
import UUID


type alias Model =
    { tree : ItemTree
    , dragDrop : Html5.DragDrop.Model ItemRef (List ItemId)
    , csrf : String
    }


type alias ItemTree =
    List ItemModel


type alias NormalData =
    { id : ItemId
    , title : String
    , completed : Bool
    , pendingCompleted : Bool
    , tags : List TagId
    , children : List ItemModel
    , expanded : Bool
    , editing : Maybe EditingData
    }


type alias EditingData =
    { inputTitle : String
    , title : Maybe String
    , tags : List TagId
    }


validateTitle : String -> Maybe String
validateTitle str =
    if String.isEmpty str then
        Nothing

    else
        Just str


type alias Nonce =
    Int


type ItemModel
    = Unloaded ItemId
    | Brief ItemBrief
    | Normal NormalData
    | New
        { nonce : Nonce
        , editing : EditingData
        }


itemId : ItemModel -> IdOrNonce
itemId mod =
    case mod of
        Unloaded id ->
            Id id

        Brief brief ->
            Id brief.id

        Normal item ->
            Id item.id

        New new ->
            Nonce new.nonce


type IdOrNonce
    = Id ItemId
    | Nonce Nonce


type alias ItemRef =
    { crumbs : List ItemId
    , id : IdOrNonce
    }


getItem : ItemTree -> IdOrNonce -> Maybe ItemModel
getItem items id =
    findFirstMatch (\item -> itemId item == id) items


getNormalItem : ItemTree -> ItemId -> Maybe NormalData
getNormalItem lst id =
    getItem lst (Id id)
        |> Maybe.andThen
            (\mod ->
                case mod of
                    Normal item ->
                        Just item

                    _ ->
                        Nothing
            )


updateNormalItem : ItemTree -> ItemId -> (NormalData -> ItemModel) -> ItemTree
updateNormalItem tree id f =
    updateFirstMatch
        (\mod ->
            case mod of
                Normal item ->
                    if item.id == id then
                        Just (f item)

                    else
                        Nothing

                _ ->
                    Nothing
        )
        tree


getItemNested : ItemTree -> ItemRef -> Maybe ItemModel
getItemNested tree ref =
    case ref.crumbs of
        [] ->
            getItem tree ref.id

        crumb :: rest ->
            getNormalItem tree crumb
                |> Maybe.andThen
                    (\item -> getItemNested item.children { ref | crumbs = rest })


deleteItemNested : ItemTree -> ItemRef -> ItemTree
deleteItemNested tree ref =
    case ref.crumbs of
        [] ->
            deleteFirstMatch (\item -> itemId item == ref.id) tree

        crumb :: rest ->
            updateNormalItem tree
                crumb
                (\item ->
                    Normal
                        { item
                            | children = deleteItemNested item.children { ref | crumbs = rest }
                        }
                )


appendItemsNested : ItemTree -> List ItemId -> List ItemModel -> ItemTree
appendItemsNested tree crumbs new =
    case crumbs of
        [] ->
            tree ++ new

        crumb :: rest ->
            updateNormalItem tree
                crumb
                (\item -> Normal { item | children = appendItemsNested item.children rest new })


getItemWithParent : ItemTree -> ItemRef -> Maybe ( ItemModel, Maybe NormalData )
getItemWithParent =
    let
        go parent tree ref =
            case ref.crumbs of
                [] ->
                    getItem tree ref.id
                        |> Maybe.map (\item -> ( item, parent ))

                crumb :: rest ->
                    getNormalItem tree crumb
                        |> Maybe.andThen
                            (\item -> go (Just item) item.children { ref | crumbs = rest })
    in
    go Nothing


updateItemNested : ItemTree -> ItemRef -> (ItemModel -> ItemModel) -> ItemTree
updateItemNested tree0 ref0 f =
    let
        go tree ref =
            case ref.crumbs of
                parent :: rest ->
                    tree
                        |> updateFirstMatch
                            (\mod ->
                                case mod of
                                    Normal item ->
                                        if item.id == parent then
                                            Just (Normal { item | children = go item.children { ref | crumbs = rest } })

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )

                [] ->
                    tree
                        |> updateFirstMatch
                            (\item ->
                                if itemId item == ref.id then
                                    Just (f item)

                                else
                                    Nothing
                            )
    in
    go tree0 ref0


updateUnloadedOrBriefItemNested : ItemTree -> ItemRef -> ItemModel -> ItemTree
updateUnloadedOrBriefItemNested tree ref new =
    updateItemNested tree ref <|
        \mod ->
            case mod of
                Unloaded _ ->
                    new

                Brief _ ->
                    new

                Normal _ ->
                    mod

                New _ ->
                    mod


updateNormalItemNested : ItemTree -> ItemRef -> (NormalData -> ItemModel) -> ItemTree
updateNormalItemNested tree ref f =
    updateItemNested tree ref <|
        \mod ->
            case mod of
                Unloaded _ ->
                    mod

                Brief _ ->
                    mod

                New _ ->
                    mod

                Normal item ->
                    f item


updateNewItemNested : ItemTree -> ItemRef -> (Nonce -> EditingData -> ItemModel) -> ItemTree
updateNewItemNested tree ref f =
    updateItemNested tree ref <|
        \mod ->
            case mod of
                Unloaded _ ->
                    mod

                Brief _ ->
                    mod

                Normal _ ->
                    mod

                New item ->
                    f item.nonce item.editing


completeChildren : List ItemModel -> Maybe Int
completeChildren =
    List.foldl
        (\mod ->
            Maybe.andThen
                (\n ->
                    case mod of
                        Unloaded _ ->
                            Nothing

                        Brief brief ->
                            Just <|
                                if brief.completed then
                                    n + 1

                                else
                                    n

                        Normal item ->
                            Just <|
                                if item.completed then
                                    n + 1

                                else
                                    n

                        New _ ->
                            Just n
                )
        )
        (Just 0)


findUnusedNonce : ItemTree -> Nonce
findUnusedNonce =
    List.foldl
        (\mod n ->
            case mod of
                New { nonce } ->
                    max (nonce + 1) n

                Normal { children } ->
                    max (findUnusedNonce children) n

                _ ->
                    n
        )
        0


hasEditingChildren : NormalData -> Bool
hasEditingChildren item =
    item.children
        |> List.any
            (\mod ->
                case mod of
                    Normal child ->
                        child.editing /= Nothing || hasEditingChildren child

                    Brief _ ->
                        False

                    Unloaded _ ->
                        False

                    New _ ->
                        True
            )


type Msg
    = NoOp
    | GotPending (List ItemId) Item
    | ExpandClicked ItemRef
    | CollapseClicked ItemRef
    | CompleteSubmitted ItemRef
    | CompleteUpdated (List ItemId) Item
    | EditClicked ItemRef
    | EditUpdated ItemRef EditingData
    | EditCancelled ItemRef
    | EditSubmitted (List ItemId) ItemId ItemInsert
    | Edited (List ItemId) Item
    | DeleteClicked (List ItemId) ItemId
    | Deleted (List ItemId) ItemId
    | NewClicked (List ItemId)
    | NewUpdated ItemRef EditingData
    | NewCancelled ItemRef
    | NewSubmitted (List ItemId) Nonce ItemInsert
    | Added (List ItemId) Nonce Item
    | Reparented ItemRef (List ItemId)
    | DragDropMsg (Html5.DragDrop.Msg ItemRef (List ItemId))


init : String -> List ItemBrief -> ( Model, Cmd Msg )
init csrf items =
    ( { tree = List.map Brief items
      , dragDrop = Html5.DragDrop.init
      , csrf = csrf
      }
    , items
        |> List.map
            (\brief ->
                getApiItemsByItem_id csrf brief.id <|
                    withResult (\_ -> NoOp) (GotPending [])
            )
        |> Cmd.batch
    )


normalItemFromItem : Item -> NormalData
normalItemFromItem item =
    { id = item.id
    , title = item.title
    , completed = item.completed
    , pendingCompleted = item.completed
    , tags = item.tags
    , children = List.map Unloaded item.children
    , expanded = False
    , editing = Nothing
    }


type TreeUpdate
    = Expanded ItemRef
    | ChildrenChanged ItemRef


requestChildrenOf : String -> List ItemId -> ItemModel -> Cmd Msg
requestChildrenOf csrf crumbs mod =
    case mod of
        Normal item ->
            item.children
                |> List.filterMap
                    (\child ->
                        case child of
                            Normal _ ->
                                Nothing

                            Brief { id } ->
                                Just id

                            Unloaded id ->
                                Just id

                            New _ ->
                                Nothing
                    )
                |> List.map
                    (\id ->
                        getApiItemsByItem_id csrf
                            id
                            (withResult (\_ -> NoOp) (GotPending (crumbs ++ [ item.id ])))
                    )
                |> Cmd.batch

        _ ->
            Cmd.none


requestChildrenAfterUpdate : String -> ItemTree -> TreeUpdate -> Cmd Msg
requestChildrenAfterUpdate csrf tree upd =
    case upd of
        Expanded ref ->
            case getItemNested tree ref of
                Just (Normal item) ->
                    item.children
                        |> List.map (requestChildrenOf csrf (ref.crumbs ++ [ item.id ]))
                        |> Cmd.batch

                _ ->
                    Cmd.none

        ChildrenChanged ref ->
            case getItemWithParent tree ref of
                Just ( mod, Just parent ) ->
                    if parent.expanded then
                        requestChildrenOf csrf ref.crumbs mod

                    else
                        Cmd.none

                Just ( mod, Nothing ) ->
                    requestChildrenOf csrf ref.crumbs mod

                Nothing ->
                    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPending crumbs item ->
            let
                ref =
                    { crumbs = crumbs, id = Id item.id }

                newTree =
                    updateUnloadedOrBriefItemNested model.tree
                        ref
                        (Normal (normalItemFromItem item))
            in
            ( { model | tree = newTree }, requestChildrenAfterUpdate model.csrf newTree (ChildrenChanged ref) )

        ExpandClicked ref ->
            let
                newTree =
                    updateNormalItemNested model.tree ref (\item -> Normal { item | expanded = True })
            in
            ( { model | tree = newTree }, requestChildrenAfterUpdate model.csrf newTree (Expanded ref) )

        CollapseClicked ref ->
            ( { model
                | tree =
                    updateNormalItemNested model.tree
                        ref
                        (\item ->
                            Normal
                                { item
                                    | expanded = False
                                    , children =
                                        item.children
                                            |> List.map
                                                (\mod ->
                                                    case mod of
                                                        Unloaded _ ->
                                                            mod

                                                        Brief _ ->
                                                            mod

                                                        New _ ->
                                                            mod

                                                        Normal child ->
                                                            Normal
                                                                { child
                                                                    | children =
                                                                        child.children
                                                                            |> List.filterMap
                                                                                (\childChild ->
                                                                                    case itemId childChild of
                                                                                        Id id ->
                                                                                            Just (Unloaded id)

                                                                                        Nonce _ ->
                                                                                            Nothing
                                                                                )
                                                                }
                                                )
                                }
                        )
              }
            , Cmd.none
            )

        CompleteSubmitted ref ->
            ( { model
                | tree =
                    updateItemNested model.tree
                        ref
                        (\mod ->
                            case mod of
                                Normal item ->
                                    Normal { item | pendingCompleted = not item.pendingCompleted }

                                _ ->
                                    mod
                        )
              }
            , case getItemNested model.tree ref of
                Just (Normal item) ->
                    putApiItemsByItem_id model.csrf item.id
                        { title = item.title
                        , tags = item.tags
                        , completed = not item.pendingCompleted
                        }
                        (withResult (\_ -> NoOp) (CompleteUpdated ref.crumbs))

                _ ->
                    Cmd.none
            )

        CompleteUpdated crumbs item ->
            let
                ref =
                    { crumbs = crumbs, id = Id item.id }

                newTree =
                    updateItemNested model.tree
                        ref
                        (\mod ->
                            let
                                new =
                                    normalItemFromItem item
                            in
                            case mod of
                                Normal old ->
                                    Normal
                                        { new
                                            | expanded = old.expanded
                                            , children = old.children
                                            , editing = old.editing
                                        }

                                _ ->
                                    Normal new
                        )
            in
            ( { model | tree = newTree }, requestChildrenAfterUpdate model.csrf newTree (ChildrenChanged ref) )

        EditClicked ref ->
            ( { model
                | tree =
                    updateNormalItemNested model.tree
                        ref
                        (\item ->
                            Normal
                                { item
                                    | editing =
                                        Just
                                            { inputTitle = item.title
                                            , title = Just item.title
                                            , tags = item.tags
                                            }
                                }
                        )
              }
            , Cmd.none
            )

        EditUpdated ref editing ->
            ( { model
                | tree =
                    updateNormalItemNested model.tree
                        ref
                        (\item -> Normal { item | editing = Just editing })
              }
            , Cmd.none
            )

        EditCancelled ref ->
            ( { model
                | tree =
                    updateNormalItemNested model.tree
                        ref
                        (\item -> Normal { item | editing = Nothing })
              }
            , Cmd.none
            )

        EditSubmitted crumbs id ins ->
            ( model
            , putApiItemsByItem_id model.csrf id ins (withResult (\_ -> NoOp) (Edited crumbs))
            )

        Edited crumbs item ->
            let
                ref =
                    { crumbs = crumbs, id = Id item.id }

                new =
                    normalItemFromItem item

                newTree =
                    updateNormalItemNested model.tree
                        ref
                        (\old ->
                            Normal
                                { new
                                    | children = old.children
                                    , expanded = old.expanded
                                }
                        )
            in
            ( { model | tree = newTree }, Cmd.none )

        DeleteClicked crumbs id ->
            ( model
            , deleteApiItemsByItem_id model.csrf id (withResult (\_ -> NoOp) (\_ -> Deleted crumbs id))
            )

        Deleted crumbs id ->
            let
                ref =
                    { crumbs = crumbs, id = Id id }
            in
            ( { model
                | tree =
                    case getItemNested model.tree ref of
                        Just (Normal { children }) ->
                            appendItemsNested (deleteItemNested model.tree ref) ref.crumbs children

                        _ ->
                            deleteItemNested model.tree ref
              }
            , Cmd.none
            )

        NewClicked crumbs ->
            ( { model
                | tree =
                    appendItemsNested model.tree
                        crumbs
                        [ New
                            { nonce = findUnusedNonce model.tree
                            , editing =
                                { inputTitle = ""
                                , title = validateTitle ""
                                , tags = []
                                }
                            }
                        ]
              }
            , Cmd.none
            )

        NewUpdated ref editing ->
            ( { model
                | tree =
                    updateNewItemNested model.tree
                        ref
                        (\nonce _ -> New { nonce = nonce, editing = editing })
              }
            , Cmd.none
            )

        NewCancelled ref ->
            ( { model | tree = deleteItemNested model.tree ref }
            , Cmd.none
            )

        NewSubmitted crumbs nonce ins ->
            ( model
            , postApiItems model.csrf ins (withResult (\_ -> NoOp) (Added crumbs nonce))
            )

        Added crumbs nonce item ->
            ( { model
                | tree =
                    updateItemNested model.tree
                        { crumbs = crumbs, id = Nonce nonce }
                        (\_ -> Normal (normalItemFromItem item))
              }
            , case last crumbs of
                Just parentId ->
                    patchApiItemsByItem_idParent model.csrf item.id (Just parentId) (\_ -> NoOp)

                Nothing ->
                    Cmd.none
            )

        Reparented from to ->
            ( { model
                | tree =
                    case getItemNested model.tree from of
                        Just item ->
                            appendItemsNested (deleteItemNested model.tree from) to [ item ]

                        Nothing ->
                            model.tree
              }
            , Cmd.none
            )

        DragDropMsg ddMsg ->
            case Html5.DragDrop.update ddMsg model.dragDrop of
                ( dragDrop, Nothing ) ->
                    ( { model | dragDrop = dragDrop }, Cmd.none )

                ( dragDrop, Just ( from, to, _ ) ) ->
                    case getItemNested model.tree from of
                        Just (New new) ->
                            ( { model
                                | dragDrop = dragDrop
                                , tree = appendItemsNested (deleteItemNested model.tree from) to [ New new ]
                              }
                            , Cmd.none
                            )

                        Just (Normal item) ->
                            ( { model
                                | dragDrop = dragDrop
                              }
                            , patchApiItemsByItem_idParent model.csrf item.id (last to) (withResult (\_ -> NoOp) (\_ -> Reparented from to))
                            )

                        _ ->
                            ( { model
                                | dragDrop = dragDrop
                              }
                            , Cmd.none
                            )


itemTagsView : List Tag -> List TagId -> List (Html Msg)
itemTagsView tagList tags =
    tags
        |> List.filterMap
            (\id -> findFirstMatch (\tag -> tag.id == id) tagList)
        |> List.map
            (\tag ->
                span
                    [ style "color" (Tags.formatColor tag.color) ]
                    [ text tag.name ]
            )


itemTagEditingView : List Tag -> EditingData -> (EditingData -> msg) -> List (Html msg)
itemTagEditingView tagList edit onEdit =
    List.concatMap
        (\id ->
            case findFirstMatch (\tag -> tag.id == id) tagList of
                Nothing ->
                    []

                Just tag ->
                    [ span
                        [ style "color" (Tags.formatColor tag.color) ]
                        [ text tag.name ]
                    , button
                        [ onClick (onEdit { edit | tags = deleteFirstMatch (\i -> i == id) edit.tags }) ]
                        [ i [ class "fa fa-times" ] [] ]
                    ]
        )
        edit.tags
        ++ [ select
                [ onInput
                    (\idString ->
                        onEdit
                            { edit
                                | tags =
                                    edit.tags
                                        ++ (case UUID.fromString idString of
                                                Ok uuid ->
                                                    [ TagId uuid ]

                                                Err _ ->
                                                    []
                                           )
                            }
                    )
                ]
                (option [ selected True, disabled True ] [ text "-- Tag --" ]
                    :: List.filterMap
                        (\tag ->
                            if List.any (\id -> id == tag.id) edit.tags then
                                Nothing

                            else
                                Just
                                    (option
                                        [ value
                                            (case tag.id of
                                                TagId uuid ->
                                                    UUID.toString uuid
                                            )
                                        ]
                                        [ text tag.name ]
                                    )
                        )
                        tagList
                )
           ]


expandCollapseView : { expanded : Bool, disabled : Bool } -> ItemRef -> Html Msg
expandCollapseView args ref =
    button
        [ onClick
            (if args.expanded then
                CollapseClicked ref

             else
                ExpandClicked ref
            )
        , disabled args.disabled
        ]
        [ if args.expanded then
            i [ class "fa fa-caret-down" ] []

          else
            i [ class "fa fa-caret-right" ] []
        ]


completedView : { completed : Bool, pendingCompleted : Bool } -> ItemRef -> Html Msg
completedView { completed, pendingCompleted } ref =
    input
        [ checked completed
        , disabled (completed /= pendingCompleted)
        , type_ "checkbox"
        , onClick (CompleteSubmitted ref)
        ]
        []


completeChildrenView : List ItemModel -> Html msg
completeChildrenView children =
    let
        completeChildrenText =
            case completeChildren children of
                Just n ->
                    String.fromInt n

                Nothing ->
                    "..."

        completionText =
            completeChildrenText
                ++ "/"
                ++ String.fromInt (List.length children)
    in
    span [] [ text completionText ]


editTitleView : EditingData -> (EditingData -> msg) -> Html msg
editTitleView edit onEdit =
    input
        [ type_ "text"
        , onInput
            (onEdit
                << (\title -> { edit | inputTitle = title, title = validateTitle title })
            )
        , value edit.inputTitle
        ]
        []


submitView : EditingData -> Bool -> (ItemInsert -> msg) -> Html msg
submitView edit completed onSubmit =
    button
        (case edit.title of
            Just title ->
                [ onClick (onSubmit { title = title, tags = edit.tags, completed = completed }) ]

            _ ->
                [ disabled True ]
        )
        [ i [ class "fa fa-save" ] [] ]


dragView : ItemRef -> Html Msg
dragView ref =
    i (class "fa fa-grip-horizontal" :: Html5.DragDrop.draggable DragDropMsg ref) []


normalItemSelfView : List Tag -> Html5.DragDrop.Model ItemRef (List ItemId) -> List ItemId -> NormalData -> Html Msg
normalItemSelfView tagList dragDrop crumbsRev item =
    let
        ref =
            { crumbs = List.reverse crumbsRev, id = Id item.id }

        attrs =
            dragAttrs ref dragDrop
                ++ dropAttrs (ref.crumbs ++ [ item.id ]) dragDrop
                ++ Html5.DragDrop.droppable DragDropMsg (ref.crumbs ++ [ item.id ])
    in
    case item.editing of
        Nothing ->
            div attrs
                [ dragView ref
                , expandCollapseView { expanded = item.expanded, disabled = hasEditingChildren item } ref
                , completedView { completed = item.completed, pendingCompleted = item.pendingCompleted } ref
                , span [] [ text item.title ]
                , completeChildrenView item.children
                , span [] (itemTagsView tagList item.tags)
                , button [ onClick (EditClicked ref) ] [ i [ class "fa fa-edit" ] [] ]
                ]

        Just editing ->
            div attrs
                [ dragView ref
                , expandCollapseView { expanded = item.expanded, disabled = hasEditingChildren item } ref
                , completedView { completed = item.completed, pendingCompleted = item.pendingCompleted } ref
                , editTitleView editing (EditUpdated ref)
                , completeChildrenView item.children
                , span [] (itemTagEditingView tagList editing (EditUpdated ref))
                , submitView editing item.completed (EditSubmitted ref.crumbs item.id)
                , button [ onClick (DeleteClicked ref.crumbs item.id) ] [ i [ class "fa fa-trash-alt" ] [] ]
                , button [ onClick (EditCancelled ref) ] [ i [ class "fa fa-times-circle" ] [] ]
                ]


newItemView : List Tag -> Html5.DragDrop.Model ItemRef (List ItemId) -> List ItemId -> Nonce -> EditingData -> Html Msg
newItemView tagList dragDrop crumbsRev nonce editing =
    let
        ref =
            { crumbs = List.reverse crumbsRev, id = Nonce nonce }
    in
    div (dragAttrs ref dragDrop)
        [ span (Html5.DragDrop.draggable DragDropMsg ref) [ text ":::" ]
        , completedView { completed = False, pendingCompleted = True } ref
        , editTitleView editing (NewUpdated ref)
        , completeChildrenView []
        , span [] (itemTagEditingView tagList editing (NewUpdated ref))
        , submitView editing False (NewSubmitted ref.crumbs nonce)
        , button [ onClick (NewCancelled ref) ] [ i [ class "fa fa-times-circle" ] [] ]
        ]


dropAttrs : List ItemId -> Html5.DragDrop.Model ItemRef (List ItemId) -> List (Attribute msg)
dropAttrs myCrumbs dragDrop =
    case Html5.DragDrop.getDropId dragDrop of
        Just dropCrumbs ->
            if dropCrumbs == myCrumbs then
                [ style "background" "#0C0" ]

            else
                case Html5.DragDrop.getDragId dragDrop of
                    Just _ ->
                        [ style "background" "#CCC" ]

                    Nothing ->
                        []

        Nothing ->
            case Html5.DragDrop.getDragId dragDrop of
                Just _ ->
                    [ style "background" "#CCC" ]

                Nothing ->
                    []


dragAttrs : ItemRef -> Html5.DragDrop.Model ItemRef (List ItemId) -> List (Attribute msg)
dragAttrs myRef dragDrop =
    case Html5.DragDrop.getDragId dragDrop of
        Just dragRef ->
            if dragRef == myRef then
                [ style "opacity" "33%" ]

            else
                []

        Nothing ->
            []


view : List Tag -> Model -> List (Html Msg)
view tags model =
    let
        go crumbsRev items =
            List.map
                (\mod ->
                    div [] <|
                        case mod of
                            Unloaded _ ->
                                [ div [] [ text "..." ]
                                ]

                            Brief item ->
                                [ div []
                                    [ input [ checked item.completed, disabled True, type_ "checkbox" ] []
                                    , span [] [ text item.title ]
                                    , span [] [ text "..." ]
                                    ]
                                ]

                            Normal item ->
                                if item.expanded then
                                    [ normalItemSelfView tags model.dragDrop crumbsRev item
                                    , div [ style "display" "flex" ]
                                        [ text "\u{00A0}"
                                        , span [ style "flex" "1" ]
                                            (go (item.id :: crumbsRev) item.children)
                                        ]
                                    ]

                                else
                                    [ normalItemSelfView tags model.dragDrop crumbsRev item ]

                            New new ->
                                [ newItemView tags model.dragDrop crumbsRev new.nonce new.editing
                                ]
                )
                items
                ++ [ button [ onClick (NewClicked (List.reverse crumbsRev)) ] [ i [ class "fa fa-plus" ] [] ] ]
    in
    h2 (dropAttrs [] model.dragDrop ++ Html5.DragDrop.droppable DragDropMsg []) [ text "Items" ]
        :: go [] model.tree
