module Urb.Urb
    exposing
        ( Model
        , Msg(..)
        , update
        , init
        , emptyUrb
        , sendPoke
        , sendSub
        , poll
        , getErrorPayload
        , getErrorDesc
        )

{-| Urb connects your application to Urbit ship.
@docs Model, Msg, update, init, emptyUrb
@docs sendPoke
@docs sendSub, poll
@docs getErrorPayload, getErrorDesc
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
import Urb.Conn exposing (..)
import Urb.Validator exposing (..)
import Urb.Mark exposing (..)


{-| Urb connector state.
-}
type alias Model msg b =
    { auth : AuthPayload
    , ship : Ship
    , error : Maybe ErrResponse
    , eventId : Int
    , isPooling : Bool
    , authOptions : AuthOptions
    , toMsg : Msg -> msg
    , codecs : List (Codec b)
    }


{-| initial Urb state
-}
emptyUrb : (Msg -> msg) -> List (Codec b) -> Model msg b
emptyUrb toMsg codecs =
    { auth = defaultAuth
    , authOptions = defaultOptions
    , ship = emptyShip
    , error = Nothing
    , eventId = 1
    , isPooling = False
    , toMsg = toMsg
    , codecs = codecs
    }


{-| Urb bootup.
-}
init : (Msg -> msg) -> List (Codec b) -> ( Model msg b, Cmd msg )
init toMsg codecs =
    let
        urb =
            emptyUrb toMsg codecs
    in
        ( urb, requestInitialAuth urb )


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


{-| Update Urbit state machine.
-}
update : Msg -> Model m b -> ( Model m b, Cmd m )
update msg model =
    case msg of
        Authorize ->
            ( model, requestInitialAuth model )

        InitAuthResponse (Ok payload) ->
            ( { model | auth = payload, error = Nothing }
            , requestAuthAs model payload.ship
            )

        InitAuthResponse (Err err) ->
            ( { model | error = Just <| fromHttpError err }, Cmd.none )

        AuthResponse (Ok payload) ->
            ( { model
                | auth = payload
                , ship =
                    Result.withDefault emptyShip (shipFromAuth payload)
              }
            , Cmd.none
            )

        AuthResponse (Err err) ->
            -- Try to authenticate as anon
            if model.authOptions.allowAnon then
                ( model, requestAuthAsAnon model )
            else
                ( { model | error = Just <| fromHttpError err }, Cmd.none )

        AnonAuthResponse (Ok payload) ->
            ( { model
                | auth = payload
                , ship = Result.withDefault emptyShip (shipFromAnonAuth payload)
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
                , poll model
                )

        PollResponse (Ok str) ->
            case (D.decodeString decodePollBeat str) of
                -- Do not increase event Id on beat
                Ok _ ->
                    ( model, poll model )

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


requestInitialAuth : Model msg b -> Cmd msg
requestInitialAuth model =
    Cmd.map model.toMsg <|
        Http.send InitAuthResponse (Http.get "/~/auth.json" decodeAuthPayload)


requestAuthAs : Model msg b -> String -> Cmd msg
requestAuthAs model shipName =
    Cmd.map model.toMsg <|
        Http.send AuthResponse (authGet shipName)


requestAuthAsAnon : Model msg b -> Cmd msg
requestAuthAsAnon model =
    Cmd.map model.toMsg <|
        Http.send AnonAuthResponse authGetAnon


{-| Poke urbit ship
-}
sendPoke : Model msg b -> Poke -> Cmd msg
sendPoke urb poke =
    Cmd.map urb.toMsg <|
        Http.send PokeResponse (post (pokeUrl poke) (Http.jsonBody (pokePayload urb.auth poke)) decodePokePayload)


{-| Urbit subscription interface
-}
sendSub : Model msg b -> Subs -> SubsAction -> Cmd msg
sendSub urb sub act =
    Cmd.map urb.toMsg <|
        Http.send SubsResponse
            (post (subsUrl urb.auth sub act)
                (Http.jsonBody (subsPayload urb.auth sub))
                decodeSubsPayload
            )


{-| Listen for Urbit response.
-}
poll : Model msg b -> Cmd msg
poll urb =
    Cmd.map urb.toMsg <|
        Http.send PollResponse
            (getString (pollUrl urb.auth urb.eventId) D.decodeString)


{-| Maybe return Urbit error}
-}
getErrorPayload : Model msg b -> Maybe ErrPayload
getErrorPayload model =
    case model.error of
        Just err ->
            err.payload

        Nothing ->
            Nothing


{-| extract last error description
-}
getErrorDesc : Model msg b -> String
getErrorDesc model =
    case model.error of
        Just err ->
            err.desc

        Nothing ->
            ""
