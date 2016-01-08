module Tests (..) where

import ElmTest exposing (..)
import TypeExtractor exposing (extractTypes)


all : Test
all =
    suite
        "A Test Suite"
        [ test "Parse a string with just one typedef" stringWithOneTypeDef
        , test "Parse a string with two typedefs" stringWithTwoTypeDefs
        , test "Parse a string of more complex code" complexTypes
        ]


stringWithOneTypeDef : Assertion
stringWithOneTypeDef =
    let
        res = extractTypes """
foo : String
"""

        expected = [ {identifier = "foo", type' = "String"} ]
    in
        case res of
            Ok ast ->
                (assertEqual expected ast)

            Err str ->
                assertEqual "" str


stringWithTwoTypeDefs : Assertion
stringWithTwoTypeDefs =
    let
        res = extractTypes """
foo : String
mickey: Mouse
"""

        expected =
            [ {identifier = "foo", type' = "String"}
            , {identifier = "mickey", type' = "Mouse"}
            ]
    in
        case res of
            Ok ast ->
                (assertEqual expected ast)

            Err str ->
                assertEqual "" str


complexTypes : Assertion
complexTypes =
    let
        res = extractTypes """
foo : String -> (String -> Number) -> Number
foo str fn =
    5

mickey: Mouse
mickey =
    { name = "Mickey"
    , occupation = "Mouse"
    }
"""

        expected =
            [ {identifier = "foo", type' = "String -> (String -> Number) -> Number"}
            , {identifier = "mickey", type' = "Mouse"}
            ]
    in
        case res of
            Ok ast ->
                (assertEqual expected ast)

            Err str ->
                assertEqual "" str
