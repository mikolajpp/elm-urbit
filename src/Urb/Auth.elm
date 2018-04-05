module Urb.Auth
    exposing
        ( Auth
        , AuthPayload
        , decodeAuthPayload
        , defaultAuth
        , AuthOptions
        , defaultOptions
        , shipFromAuth
        , shipFromAnonAuth
        )

{-| #Auth
This module defines data structures needed for Urbit authentication.
Any request to the Urbit API requires valid
authentication credentials.


## Authentication data structures

@docs Auth, AuthPayload, AuthOptions, defaultOptions, defaultAuth, decodeAuthPayload
@docs shipFromAuth, shipFromAnonAuth

-}

import Http
import Json.Decode as D
import Json.Encode as E
import Regex exposing (find, regex)
import List exposing (..)
import Dict exposing (Dict)
import Urb.Ship exposing (..)


{-| Urbit authentication payload.
-}
type alias AuthPayload =
    { oryx : String
    , user : String
    , sein : String
    , ixor : String
    , ship : String
    , auth : List String
    }


{-| Different kinds of authentication schemes
-}
type Auth
    = Self
    | Anon
    | Remote Ship


{-| Authentication options.

  - allowAnon - Allow authenticating as anonymous ship.

-}
type alias AuthOptions =
    { allowAnon : Bool
    }


{-| Default authentication options.
-}
defaultOptions : AuthOptions
defaultOptions =
    { allowAnon = True
    }


{-| Urbit authentication payload.
-}
defaultAuth : AuthPayload
defaultAuth =
    { oryx = ""
    , user = ""
    , sein = ""
    , ixor = ""
    , ship = ""
    , auth = []
    }


{-| Decode JSON auth response into AuthPayload.
-}
decodeAuthPayload : D.Decoder AuthPayload
decodeAuthPayload =
    D.map6
        AuthPayload
        (D.field "oryx" D.string)
        (D.field "user" D.string)
        (D.field "sein" D.string)
        (D.field "ixor" D.string)
        (D.field "ship" D.string)
        (D.field "auth" (D.list D.string))


{-| Obtains ship from received authentication payload.
-}
shipFromAuth : AuthPayload -> Result String Ship
shipFromAuth payload =
    shipFromString (Maybe.withDefault "" (head payload.auth))


{-| Obtains anonymous ship from received authentication payload.
-}
shipFromAnonAuth : AuthPayload -> Result String Ship
shipFromAnonAuth payload =
    shipFromString (Maybe.withDefault "" <| Just payload.user)
