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

{-|


# Auth - Urbit authorization

In order to be able to perform any requests to an Urbit shit,
a user must obtain credentials first. Urbit generally supports
three different kind of authorization schemes:

  - Anonymous
  - Self
  - Remote

Anonymous credentials do not require valid password
an can be obtained by anyone, essentially giving user
a fake comet identity that can still be used for many tasks,
such as accessing publicly available services.

Self authorization gives user full access to an urbit ship
and requires knowledge of the secret key.

(*deprecated/not working/instructions unclear*)
Lastly, it is possible to become
authenticated as a remote ship. The authorizing ship will
then perform the call to the remote ship and if the remote
ship confirms supplied credentials are valid,
the user is granted remote's ship identity.


## Handling authentication

@docs Auth, AuthPayload, AuthOptions, defaultOptions, defaultAuth, decodeAuthPayload
@docs shipFromAuth, shipFromAnonAuth

-}

import Http
import Json.Decode as D
import Json.Encode as E
import List exposing (..)
import Dict exposing (Dict)
import Urb.Ship exposing (..)


{-|


## Authentication payload

This is the structure that
is returned by the urbit ship upon
successful authentication request.

@oryx - secret cookie
@user - fake comet's identity
@sein - (??)
@ixor - hash of something (?)
@ship - authorizing ship
@auth - list of obtained identities

-}
type alias AuthPayload =
    { oryx : String
    , user : String
    , sein : String
    , ixor : String
    , ship : String
    , auth : List String
    }


{-|


## Kinds of authentication schemes

@Self - login with ship's identity.
@Anon - obtain anonymous comet's identity
@Remote - login using remote ship's identity.

-}
type Auth
    = Self
    | Anon
    | Remote Ship


{-|


## Authentication options

@allowAnon - if set, upon failed authentication using
`Self` or `Remote` Urb will try to obtain anonymous identity.

Set to false if you don't want to disallow anonymous logins
in your application.

-}
type alias AuthOptions =
    { allowAnon : Bool
    }


{-|


## Default authentication options

Anonymous logins are forbidden by default.

-}
defaultOptions : AuthOptions
defaultOptions =
    { allowAnon = False
    }


{-| Empty authentication payload.
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


{-| Obtains ship from received authentication data.
-}
shipFromAuth : AuthPayload -> Result String Ship
shipFromAuth payload =
    shipFromString (Maybe.withDefault "" (head payload.auth))


{-| Obtains anonymous ship from received authentication payload.
-}
shipFromAnonAuth : AuthPayload -> Result String Ship
shipFromAnonAuth payload =
    shipFromString (Maybe.withDefault "" <| Just payload.user)
