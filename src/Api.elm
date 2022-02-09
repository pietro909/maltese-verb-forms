module Api exposing (getVerb)

import Http
import Json.Decode exposing (Decoder, andThen, at, field, index, string, succeed)


getVerb : String -> (Result Http.Error String -> msg) -> Cmd msg
getVerb mamma msg =
    let
        url =
            -- "https://cors-anywhere.herokuapp.com/"
            "https://mlrs.research.um.edu.mt/resources/gabra-api/lexemes/search?pos=VERB&s="
                ++ mamma
    in
    Http.get { url = url, expect = Http.expectJson (\r -> msg r) decodeGloss }


decodeGloss : Decoder String
decodeGloss =
    field "results" (index 0 (at [ "lexeme", "glosses" ] (index 0 (field "gloss" string))))
        |> andThen
            (\translation ->
                translation
                    |> String.split "\n"
                    |> String.join ", "
                    |> succeed
            )
