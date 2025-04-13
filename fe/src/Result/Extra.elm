module Result.Extra exposing (withResult)


withResult : (e -> r) -> (a -> r) -> Result e a -> r
withResult onErr onOk result =
    case result of
        Err err ->
            onErr err

        Ok ok ->
            onOk ok
