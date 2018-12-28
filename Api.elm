module Api exposing (getVerb)

import Http
import Json.Decode exposing (Decoder, field, at, index, string, andThen, succeed)


getVerb : (Result Http.Error String -> msg) -> String -> Cmd msg
getVerb msg mamma =
    let
        query =
            "https://cors-anywhere.herokuapp.com/"
                ++ "http://mlrs.research.um.edu.mt/resources/gabra-api/lexemes/search?pos=VERB&s="
                ++ mamma
    in
        Http.get
            { url = query
            , expect = Http.expectJson msg decodeGloss
            }


decodeGloss : Decoder String
decodeGloss =
    field "results" (index 0 (at [ "lexeme", "gloss" ] string))
        |> andThen
            (\translation ->
                translation
                    |> String.split "\n"
                    |> String.join ", "
                    |> succeed
            )
