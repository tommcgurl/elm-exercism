module Bob exposing (..)


type Response
    = Shouting
    | Asking
    | Stating
    | Silence


respondTo : Response -> String
respondTo response =
    case response of
        Shouting ->
            "Whoa, chill out!"

        Asking ->
            "Sure."

        Stating ->
            "Whatever."

        Silence ->
            "Fine. Be that way!"


hey : String -> String
hey something =
    if (String.trim something == "") then
        respondTo Silence
    else if (String.toUpper something == something) && (String.toLower something /= something) then
        respondTo Shouting
    else if (String.endsWith "?" something) then
        respondTo Asking
    else
        respondTo Stating
