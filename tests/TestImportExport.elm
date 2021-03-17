module TestImportExport exposing (..)

import Examples
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Main exposing (decodeCharacter)
import Test exposing (..)


importExportSuite : Test
importExportSuite =
    describe "Tests the importing and exporting of characters"
        [ describe "Import sheet"
            [ test "Sheet can be imported" <|
                \_ ->
                    case Decode.decodeString decodeCharacter Examples.characterV1 of
                        Ok char ->
                            Expect.equal 3 char.stats.str

                        Err e ->
                            Expect.fail <| Decode.errorToString e
            , test "Faulty Sheet can't be imported" <|
                \_ -> Expect.err <| Decode.decodeString decodeCharacter Examples.wrongCharacter
            , test "Character without Str can be imported" <|
                \_ ->
                    case Decode.decodeString decodeCharacter Examples.noStrCharacter of
                        Ok char ->
                            Expect.equal 0 char.stats.str

                        Err e ->
                            Expect.fail <| Decode.errorToString e
            , test "Character with more than 20 items can't be imported" <|
                \_ -> Expect.err <| Decode.decodeString decodeCharacter Examples.overloadedCharacter
            ]
        ]
