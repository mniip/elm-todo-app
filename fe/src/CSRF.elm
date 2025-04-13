port module CSRF exposing
    ( getCsrfToken
    , gotCsrfToken
    )

port getCsrfToken : () -> Cmd msg


port gotCsrfToken : (String -> msg) -> Sub msg
