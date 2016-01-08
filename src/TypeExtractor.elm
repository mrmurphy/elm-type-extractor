module TypeExtractor (TypeDef, extractTypes) where

{-| This small module will take a string of Elm source, and extract all top-level type declarations.

# Types
@docs TypeDef

# Functions
@docs extractTypes
-}

import Combine exposing (Parser, regex, maybe)
import Combine.Infix exposing ((<*), (<*>), (<$>))
import VerbalExpressions as Vex exposing (verex, startOfLine, endOfLine, possibly, word, followedBy, something, anything, VerbalExpression, lineBreak)
import List
import String
import Result.Extra exposing (combine)


{-| -}
type alias TypeDef =
    { identifier : String
    , type' : String
    }


maybeToList : Maybe.Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just thing ->
            [ thing ]

        Nothing ->
            []


dropNothings : List (Maybe node) -> List node
dropNothings =
    List.concatMap maybeToList


vexParser : VerbalExpression -> Parser String
vexParser ex =
    regex <| Vex.toString ex


typeDeclarationParser : Parser TypeDef
typeDeclarationParser =
    let
        ident =
            vexParser (verex |> startOfLine >> word)

        sep =
            vexParser (verex |> possibly " " >> followedBy ":" >> possibly " ")

        def =
            vexParser (verex |> something >> endOfLine)
    in
        -- Not sure how to make this code clearer. Everything to the left of
        -- <*> Is constructing a Parser (a -> a'). It needs to be a parser of
        -- a partially applied function. Then the result of the parser on the
        -- right side is passed into that partially applied function, and a
        -- parser is returned that returns the result of running that function.
        Combine.map TypeDef (ident <* sep) <*> def


moduleParser : Parser (Maybe TypeDef)
moduleParser =
    maybe typeDeclarationParser


{-| Will turn a string of Elm source into either an error message, or an Ast.
-}
extractTypes : String -> Result String (List TypeDef)
extractTypes string =
    let
        parsedThings =
            List.map
                (Combine.parse moduleParser)
                (String.split "\n" string)
    in
        List.map fst parsedThings
            |> List.map (Result.formatError (String.join ", "))
            |> combine
            |> Result.map dropNothings
