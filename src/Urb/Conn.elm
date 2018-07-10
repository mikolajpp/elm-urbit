module Urb.Conn
    exposing
        ( pollDecode
        , Codec
        , pollUrl
        , PokePayload
        , PollPayload(..)
        , SubsPayload
        , subsPayload
        , SubsAction(..)
        , Subs
        , subsUrl
        , decodeSubsPayload
        , decodePollBeat
        , Poke
        , pokeUrl
        , pokePayload
        , decodePokePayload
        )

{-| #Urbit connector.

Applications external to Urbit can use
the ansychronous message passing in order to communicate with applications
living inside Urbit.

In order to communicate with an Urbit app, you need to first subscribe
on a correct app, wire, mark connection.

After subscription, your application will receive messages in ansychronous
fashion.

To send messages to Urbit application, you can perform Pokes, which again
require specifying the app you want to talk to, as well as wire and mark.

@docs pollDecode, Codec, pollUrl, PokePayload, SubsPayload, decodePollBeat
@docs SubsAction, Poke, PokePayload, Subs, pokeUrl, pokePayload, decodePokePayload
@docs decodeSubsPayload, subsUrl, subsPayload, PollPayload

-}

import Http as Http
import Json.Decode as D
import Json.Encode as E
import Regex exposing (find, regex)
import List exposing (..)
import Dict exposing (Dict)
import String.Interpolate exposing (interpolate)
import Urb.Auth exposing (..)


{-| Urbit poke structure
-}
type alias Poke =
    { ship : String
    , app : String
    , mark : String
    , wire : String
    , xyro : E.Value
    }


{-| Poke request response
-}
type alias PokePayload =
    { ok : Bool
    }


{-| Generate poke url from a poke structure
-}
pokeUrl : Poke -> String
pokeUrl poke =
    interpolate "/~/to/{0}/{1}"
        [ poke.app, poke.mark ]


{-| Generate poke request JSON
from auth and poke data
-}
pokePayload : AuthPayload -> Poke -> E.Value
pokePayload auth poke =
    (E.object
        [ ( "oryx", E.string auth.oryx )
        , ( "wire", E.string poke.wire )
        , ( "xyro", poke.xyro )
        ]
    )


{-| Decodes poke response
-}
decodePokePayload : D.Decoder PokePayload
decodePokePayload =
    D.map PokePayload
        (D.field "ok" D.bool)


{-| Urbit subscription request
-}
type alias Subs =
    { ship : String
    , app : String
    , mark : String
    , wire : String
    }


{-| Subscription request
response
-}
type alias SubsPayload =
    { ok : Bool }


{-| Type of subscribe poke.
-}
type SubsAction
    = Subscribe
    | Unsubscribe


{-| Generate
subscription request url
-}
subsUrl : AuthPayload -> Subs -> SubsAction -> String
subsUrl auth subs action =
    let
        act =
            case action of
                Subscribe ->
                    "PUT"

                Unsubscribe ->
                    "DELETE"
    in
        interpolate "/~/is/{0}/{1}.json?{2}"
            [ subs.app, subs.wire, act ]


{-| Generates subscription request JSON
-}
subsPayload : AuthPayload -> Subs -> E.Value
subsPayload auth sub =
    (E.object
        [ ( "appl", E.string sub.app )
        , ( "mark", E.string sub.mark )
        , ( "wire", E.string ("/" ++ sub.wire) )
        , ( "ship", E.string auth.ship )
        , ( "oryx", E.string auth.oryx )
        ]
    )


{-| Decodes subscription response.
-}
decodeSubsPayload : D.Decoder SubsPayload
decodeSubsPayload =
    D.map SubsPayload
        (D.field "ok" D.bool)


{-| Poll beat payload.
-}
type alias PollBeatPayload =
    { beat : Bool }


{-| Decodes poll beat payload from JSON response.
-}
decodePollBeat : D.Decoder PollBeatPayload
decodePollBeat =
    D.map PollBeatPayload
        (D.field "beat" D.bool)


{-| Given authorization token and sequence number,
generates polling url.
-}
pollUrl : AuthPayload -> Int -> String
pollUrl auth seq =
    interpolate "/~/of/{0}?poll={1}"
        [ auth.ixor, toString seq ]


type alias PollFromPayload =
    { app : String
    , path : String
    , ship : String
    }


decodePollFromPayload =
    D.map3 PollFromPayload
        (D.field "appl" D.string)
        (D.field "path" D.string)
        (D.field "ship" D.string)


type alias PollDataPayload =
    { data : D.Value
    , from : PollFromPayload
    , id : Int
    , typ : String
    }


decodePollDataPayload =
    D.map4 PollDataPayload
        (D.at [ "data", "json" ] D.value)
        (D.field "from" decodePollFromPayload)
        (D.field "id" D.int)
        (D.field "type" D.string)


{-| There are 3 possible
poll responses. Successful poll data payload.
Poll beat indicating we have to continue polling.
Finally, on error we receive PollErr.
-}
type PollPayload
    = PollData PollDataPayload
    | PollBeat PollBeatPayload
    | PollErr String


{-| Convert received poll string into poll payload.
-}
decodePollPayload : String -> PollPayload
decodePollPayload str =
    -- Check if it is a heartbeat
    case (D.decodeString decodePollBeat str) of
        Ok beat ->
            PollBeat beat

        -- Check if we received data
        Err _ ->
            case (D.decodeString decodePollDataPayload str) of
                Ok data ->
                    PollData data

                Err e ->
                    PollErr e


{-| A poll codec
is a pair: signature regex and matching decoder.
This is used to decode data coming from the Urbit and invoke
correct decoders supplied by the user.
-}
type alias Codec b =
    ( Regex.Regex, D.Decoder b )


{-| Given a poll payload string,
a list of poll codecs (supplied by the user),
we check dispatch codecs on payload to see if it matches
any signature. If so, we use the poll codec to convert poll payload
into user desired data structure.
-}
pollDecode : String -> List (Codec b) -> Maybe (Result String b)
pollDecode pollpay codecs =
    case (decodePollPayload pollpay) of
        PollBeat _ ->
            Nothing

        PollErr _ ->
            Nothing

        PollData pay ->
            let
                validCodecs =
                    List.filter (\t -> Regex.contains (Tuple.first t) pay.from.path)
                        codecs

                decoder =
                    List.head validCodecs
            in
                case decoder of
                    Just dec ->
                        Just (D.decodeValue (Tuple.second dec) pay.data)

                    Nothing ->
                        Nothing
