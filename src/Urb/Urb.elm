module Urb.Urb
    exposing
        ( Model
        , Msg(..)
        , update
        , init
        , initWith
        , emptyUrb
        , poke
        , sendPoke
        , SubsAction(..)
        , sendSub
        , PollPayload(..)
        , PollBeatPayload
        , poll
        , decodePollBeat
        , decodePollPayload
        , pollDecode
        )

{-| Urb connects your application to Urbit ship.
@docs Model, Msg, update, init, initWith, emptyUrb
@docs poke, sendPoke
@docs SubsAction, sendSub, PollPayload, PollBeatPayload, poll, decodePollBeat, decodePollPayload, pollDecode
-}

import Http as Http
import Json.Decode as D
import Json.Encode as E
import Regex exposing (find, regex)
import List exposing (..)
import Dict exposing (Dict)
import String.Interpolate exposing (interpolate)
import Urb.Auth exposing (..)
import Urb.Error exposing (..)
import Urb.Ship exposing (..)
import Urb.Validator exposing (..)
import Urb.Mark exposing (..)


{-| Urb connector state.
-}
type alias Model =
    { auth : AuthPayload
    , ship : Ship
    , error : Maybe ErrResponse
    , eventId : Int
    , isPooling : Bool
    , authOptions : AuthOptions
    }


{-| Urbit poke structure
-}
type alias Poke =
    { ship : String
    , app : String
    , mark : String
    , wire : String
    , xyro : E.Value
    }


type alias PokePayload =
    { ok : Bool
    }


{-| Constructor for Poke
-}
poke : Ship -> String -> String -> String -> E.Value -> Poke
poke ship app wire mark xyro =
    { ship = String.join "-" ship.address
    , app = app
    , mark = mark
    , wire = wire
    , xyro = xyro
    }


pokeUrl : Poke -> String
pokeUrl poke =
    interpolate "/~/to/{0}/{1}"
        [ poke.app, poke.mark ]


pokePayload : AuthPayload -> Poke -> E.Value
pokePayload auth poke =
    (E.object
        [ ( "oryx", E.string auth.oryx )
        , ( "wire", E.string poke.wire )
        , ( "xyro", poke.xyro )
        ]
    )


decodePokePayload : D.Decoder PokePayload
decodePokePayload =
    D.map PokePayload
        (D.field "ok" D.bool)


type alias Subs =
    { ship : String
    , app : String
    , mark : String
    , wire : String
    }


type alias SubsPayload =
    { ok : Bool }


{-| Type of subscribe poke.
-}
type SubsAction
    = Subscribe
    | Unsubscribe


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


subsPayload auth sub =
    (E.object
        [ ( "appl", E.string sub.app )
        , ( "mark", E.string sub.mark )
        , ( "wire", E.string ("/" ++ sub.wire) )
        , ( "ship", E.string auth.ship )
        , ( "oryx", E.string auth.oryx )
        ]
    )


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


pollUrl auth seq =
    interpolate "/~/of/{0}?poll={1}"
        [ auth.ixor, toString seq ]


type alias PollFromPayload =
    { appl : String
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


{-| Given a poll payload string,
a list of poll codecs (supplied by the user),
we check dispatch codecs on payload to see if it matches
any signature. If so, we use the poll codec to convert poll payload
into user desired data structure.
-}
pollDecode : String -> List ( Regex.Regex, D.Decoder b ) -> Maybe (Result String b)
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


{-| initial Urb state
-}
emptyUrb : Model
emptyUrb =
    { auth = defaultAuth
    , authOptions = defaultOptions
    , ship = emptyShip
    , error = Nothing
    , eventId = 1
    , isPooling = False
    }


{-| Urb bootup.
-}
init : ( Model, Cmd Msg )
init =
    ( emptyUrb, requestInitialAuth )


{-| Urbi bootup with options
-}
initWith : AuthOptions -> ( Model, Cmd Msg )
initWith authOpt =
    let
        urb =
            { emptyUrb | authOptions = authOpt }
    in
        ( urb, requestInitialAuth )


{-| Urb Msg.
-}
type Msg
    = InitAuthResponse (Result Http.Error AuthPayload)
    | AuthResponse (Result Http.Error AuthPayload)
    | AnonAuthResponse (Result Http.Error AuthPayload)
    | PokeResponse (Result Http.Error PokePayload)
    | SubsResponse (Result Http.Error SubsPayload)
    | PollResponse (Result Http.Error String)
    | Authorize
    | Error String


{-| Urb update machine.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authorize ->
            ( model, requestInitialAuth )

        -- Authorization response --
        InitAuthResponse (Ok payload) ->
            ( { model | auth = payload, error = Nothing }, requestAuthAs payload.ship )

        InitAuthResponse (Err err) ->
            ( { model | error = Just <| fromHttpError err }, Cmd.none )

        AuthResponse (Ok payload) ->
            ( { model
                | auth = payload
                , ship =
                    Result.withDefault emptyShip (authShipFromPayload payload)
              }
            , Cmd.none
            )

        AuthResponse (Err err) ->
            -- Try to authenticate as anon
            if model.authOptions.allowAnon then
                ( model, requestAuthAsAnon )
            else
                ( { model | error = Just <| fromHttpError err }, Cmd.none )

        AnonAuthResponse (Ok payload) ->
            ( { model
                | auth = payload
                , ship = Result.withDefault emptyShip (anonAuthShipFromPayload payload)
              }
            , Cmd.none
            )

        AnonAuthResponse (Err err) ->
            ( { model | error = Just <| fromHttpError err }, Cmd.none )

        PokeResponse (Err err) ->
            ( { model | error = Just <| fromHttpError err }, Cmd.none )

        PokeResponse (Ok payload) ->
            ( model, Cmd.none )

        SubsResponse (Err err) ->
            ( { model | error = Just <| fromHttpError err }
            , Cmd.none
            )

        SubsResponse (Ok payload) ->
            if model.isPooling then
                ( model, Cmd.none )
            else
                ( { model | isPooling = True }
                , (poll model)
                )

        PollResponse (Ok str) ->
            case (D.decodeString decodePollBeat str) of
                -- Do not increase event Id on beat
                Ok _ ->
                    ( model, (poll model) )

                Err _ ->
                    let
                        newModel =
                            { model | eventId = model.eventId + 1 }
                    in
                        ( newModel, poll newModel )

        PollResponse (Err err) ->
            ( { model | error = Just <| fromHttpError err }, Cmd.none )

        Error err ->
            ( model, Cmd.none )



-- REQUESTS


get url decode =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decode
        , timeout = Nothing
        , withCredentials = True
        }


getString url decode =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }


post url body decode =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Content-Type" "application/x-www-form-urlencoded"
            ]
        , url = url
        , body = body
        , expect = Http.expectJson decode
        , timeout = Nothing
        , withCredentials = True
        }


authGet shipName =
    get (interpolate "/~/as/~{0}/~/auth.json" [ shipName ]) decodeAuthPayload


authGetAnon =
    get "/~/as/anon/~/auth.json" decodeAuthPayload


requestInitialAuth : Cmd Msg
requestInitialAuth =
    Http.send InitAuthResponse (Http.get "/~/auth.json" decodeAuthPayload)


requestAuthAs : String -> Cmd Msg
requestAuthAs shipName =
    Http.send AuthResponse (authGet shipName)


requestAuthAsAnon : Cmd Msg
requestAuthAsAnon =
    Http.send AnonAuthResponse authGetAnon


{-| Poke urbit ship
-}
sendPoke : Model -> Poke -> Cmd Msg
sendPoke urb poke =
    Http.send PokeResponse (post (pokeUrl poke) (Http.jsonBody (pokePayload urb.auth poke)) decodePokePayload)


{-| Urbit subscription interface
-}
sendSub : Model -> Subs -> SubsAction -> Cmd Msg
sendSub urb sub act =
    Http.send SubsResponse (post (subsUrl urb.auth sub act) (Http.jsonBody (subsPayload urb.auth sub)) decodeSubsPayload)


{-| Listen for Urbit response.
-}
poll : Model -> Cmd Msg
poll urb =
    Http.send PollResponse (getString (pollUrl urb.auth urb.eventId) D.decodeString)
