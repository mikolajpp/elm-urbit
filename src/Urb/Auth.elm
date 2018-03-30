module Urb.Auth
    exposing
        ( AuthPayload
        , decodeAuthPayload
        , defaultAuth
        , AuthOptions
        , defaultOptions
        )

{-| #Auth
This module defines data structures needed for Urbit authentication.
Any request to the Urbit API requires valid
authentication credentials.


## Authentication data structures

@docs AuthPayload, AuthOptions, defaultOptions, defaultAuth, decodeAuthPayload

-}

import Http
import Json.Decode as D
import Json.Encode as E
import Regex exposing (find, regex)
import List exposing (..)
import Dict exposing (Dict)


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


{-| Authentication options.

  - allowAnon - Allow authenticating as anonymous ship.

-}
type alias AuthOptions =
    { allowAnon : Bool
    }


{-| Default authentication AuthOptions.
-}
defaultOptions : AuthOptions
defaultOptions =
    { allowAnon = False
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
