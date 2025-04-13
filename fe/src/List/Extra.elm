module List.Extra exposing
    ( deleteFirstMatch
    , findFirstMatch
    , last
    , updateFirstMatch
    )


last : List a -> Maybe a
last lst =
    case lst of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


findFirstMatch : (a -> Bool) -> List a -> Maybe a
findFirstMatch p lst =
    case lst of
        x :: xs ->
            if p x then
                Just x

            else
                findFirstMatch p xs

        [] ->
            Nothing


updateFirstMatch : (a -> Maybe a) -> List a -> List a
updateFirstMatch f =
    let
        go lst =
            case lst of
                x :: xs ->
                    case f x of
                        Just y ->
                            y :: xs

                        Nothing ->
                            x :: go xs

                [] ->
                    []
    in
    go


deleteFirstMatch : (a -> Bool) -> List a -> List a
deleteFirstMatch f =
    let
        go lst =
            case lst of
                x :: xs ->
                    if f x then
                        xs

                    else
                        x :: go xs

                [] ->
                    []
    in
    go
