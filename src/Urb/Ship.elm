module Urb.Ship
    exposing
        ( Ship
        , ShipClass(..)
        , emptyShip
        , shipFromString
        , toP
        )

{-| Utilities related to Urbit identity handling.
@docs Ship, ShipClass, emptyShip, shipFromString, toP
-}

import Http
import Json.Decode as D
import Regex
import List exposing (..)
import Dict exposing (Dict)
import Urb.Validator exposing (..)
import String.Interpolate exposing (interpolate)
import Debug exposing (toString)


{-| In Urbit, each identity belong to one of the 5 classes.

  - Galaxy - 8 bit address, example `~zod`
  - Star - 16 bit address, example `~marzod`
  - Planet - typical user ship. 32 bit address. Example `~tagfun-fossep`
  - Moon - 64 bit address, belongs to parent planet. Example: `littel-ponnys-tagfun-fossep`
  - Comet - 128bit address, the Urbit 'anonymous' identity.

Locally, there also exist anonymous Web-only identity - Anon class, which can be used
to give users access to Urbit facilities through Urbit web interface.

-}
type ShipClass
    = Galaxy
    | Star
    | Planet
    | Moon
    | Comet
    | Anon


{-| Urbit ship data structure.
-}
type alias Ship =
    { class : ShipClass
    , p : String
    , address : List String
    , shortAddress : String
    }


{-| Convert ship to its Urbit `@p` string
-}
toP : Ship -> String
toP ship =
    ship.p


{-| Defines empty ship.
-}
emptyShip : Ship
emptyShip =
    { class = Comet, p = "", address = [], shortAddress = "" }


allPartsSatisfy : (String -> Bool) -> List String -> Bool
allPartsSatisfy pred parts =
    (List.length (List.filter (\s -> pred s) parts)) == List.length parts


shipClassFromParts : List String -> Result String ShipClass
shipClassFromParts parts =
    case (List.length parts) of
        1 ->
            let
                headstr =
                    Maybe.withDefault "" (head parts)
            in
                case String.length headstr of
                    3 ->
                        Ok Galaxy

                    6 ->
                        Ok Star

                    _ ->
                        Err (interpolate "Invalid part `{0}' of ship address" [ headstr ])

        c ->
            -- FIXME: Anon auth won't work on galaxies
            if not (allPartsSatisfy (\p -> String.length p == 6) parts) then
                Err ""
            else
                case c of
                    2 ->
                        Ok Planet

                    4 ->
                        Ok Moon

                    8 ->
                        Ok Comet

                    _ ->
                        if (c == 3) || (c == 5) || (c == 9) then
                            Ok Anon
                        else
                            Err
                                (interpolate
                                    "Invalid number of parts for ship address: `{0}'"
                                    [ (toString parts) ]
                                )


shortenAddress : ShipClass -> List String -> String
shortenAddress class parts =
    let
        first =
            Maybe.withDefault "" <| List.head parts

        last1 =
            Maybe.withDefault "" <| List.head <| List.drop ((List.length parts) - 2) parts

        last =
            Maybe.withDefault "" <| List.head <| List.drop ((List.length parts) - 1) parts
    in
        case class of
            Comet ->
                first ++ "_" ++ last

            Moon ->
                last1 ++ "^" ++ last

            Planet ->
                first ++ "-" ++ last

            Galaxy ->
                first

            Star ->
                first

            Anon ->
                "anonymous"


{-| Converts a string into
a Ship, or an error message if the parsing failed.
-}
shipFromString : String -> Result String Ship
shipFromString str =
    let
        rparts =
            validPartsFromStr str
    in
        case rparts of
            Ok parts ->
                case (shipClassFromParts parts) of
                    Ok class ->
                        Ok ({ address = parts, p = str, shortAddress = shortenAddress class parts, class = class })

                    Err er ->
                        Err (interpolate "Malformed ship address: `{0}'" [ er ])

            Err errors ->
                Err (String.join ", " errors)
