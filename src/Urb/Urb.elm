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
        , isPolling
        , ConnStatus(..)
        , getPollData
        )

{-| Urb connects your application to Urbit ship.
@docs Model, Msg, update, init, emptyUrb
@docs sendPoke
@docs sendSub, poll
@docs getErrorPayload, getErrorDesc, isPolling, ConnStatus, getPollData
-}

import Http as Http
import Json.Decode as D
import String.Interpolate exposing (interpolate)
import Urb.Auth exposing (..)
import Urb.Error exposing (..)
import Urb.Ship exposing (..)
import Urb.Conn exposing (..)


{-| Urb connector state.
-}
type alias Model msg b =
    { urbitUrl : String
    , auth : AuthPayload
    , ship : Ship
    , error : Maybe ErrResponse
    , eventId : Int
    , isPolling : Bool
    , authOptions : AuthOptions
    , toMsg : Msg b -> msg
    , codecs : List (Codec b)
    , connStatus : ConnStatus
    , pollData : Maybe b
    }


{-| Connection status
-}
type ConnStatus
    = Disconnected
    | Connected


{-| Whether last subscription
was successful
-}
isPolling : Model msg b -> Bool
isPolling model =
    model.isPolling


{-| Retrieve last received polling data
-}
getPollData : Model msg b -> Maybe b
getPollData model =
    model.pollData


{-| initial Urb state
-}
emptyUrb : String -> (Msg b -> msg) -> List (Codec b) -> Model msg b
emptyUrb urbiturl toMsg codecs =
    { urbitUrl = urbiturl
    , auth = defaultAuth
    , authOptions = defaultOptions
    , ship = emptyShip
    , error = Nothing
    , eventId = 1
    , isPolling = False
    , toMsg = toMsg
    , codecs = codecs
    , connStatus = Disconnected
    , pollData = Nothing
    }


{-| Urb bootup.
-}
init : String -> (Msg b -> msg) -> List (Codec b) -> ( Model msg b, Cmd msg )
init urbiturl toMsg codecs =
    let
        urb =
            emptyUrb urbiturl toMsg codecs
    in
        ( urb, requestInitialAuth urb )


{-| Urb Msg.
-}
type Msg b
    = InitAuthResponse (Result Http.Error AuthPayload)
    | AuthResponse (Result Http.Error AuthPayload)
    | AnonAuthResponse (Result Http.Error AuthPayload)
    | PokeResponse (Result Http.Error PokePayload)
    | SubsResponse (Result Http.Error SubsPayload)
    | PacketResponse (Packet b)
    | Authorize
    | Error String


{-| Update Urbit state machine.
-}
update : Msg b -> Model m b -> ( Model m b, Cmd m )
update msg model =
    case msg of
        Authorize ->
            ( model, requestInitialAuth model )

        InitAuthResponse (Ok payload) ->
            ( { model | auth = payload, error = Nothing }
            , requestAuthAs model payload.ship
            )

        InitAuthResponse (Err err) ->
            ( { model
                | error = Just <| fromHttpError err
                , connStatus = Disconnected
              }
            , Cmd.none
            )

        AuthResponse (Ok payload) ->
            ( { model
                | auth = payload
                , ship =
                    Result.withDefault emptyShip (shipFromAuth payload)
                , connStatus = Connected
              }
            , Cmd.none
            )

        AuthResponse (Err err) ->
            -- Try to authenticate as anon
            if model.authOptions.allowAnon then
                ( model, requestAuthAsAnon model )
            else
                ( { model
                    | error = Just <| fromHttpError err
                    , connStatus = Disconnected
                  }
                , Cmd.none
                )

        AnonAuthResponse (Ok payload) ->
            ( { model
                | auth = payload
                , ship = Result.withDefault emptyShip (shipFromAnonAuth payload)
                , connStatus = Connected
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
            ( { model | error = Just <| fromHttpError err, isPolling = False }
            , Cmd.none
            )

        SubsResponse (Ok payload) ->
            if model.isPolling then
                ( model, Cmd.none )
            else
                ( { model | isPolling = True }
                , poll model
                )

        PacketResponse rsp ->
            case rsp of
                PollBeat ->
                    ( model, poll model )

                Packet rpkt ->
                    case rpkt of
                        Just pkt ->
                            case pkt of
                                Ok data ->
                                    ( { model
                                        | eventId = data.id + 1
                                        , error = Nothing
                                      }
                                    , poll model
                                    )

                                Err err ->
                                    ( { model
                                        | error =
                                            Just err
                                      }
                                    , poll model
                                    )

                        Nothing ->
                            ( model, poll model )

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


postString url body =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Content-Type" "application/x-www-form-urlencoded"
            ]
        , url = url
        , body = body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }


authGet : Model msg b -> String -> Http.Request AuthPayload
authGet urb shipName =
    get (interpolate "{0}/~/as/~{1}/~/auth.json" [ urb.urbitUrl, shipName ]) decodeAuthPayload


authGetAnon : Model msg b -> Http.Request AuthPayload
authGetAnon urb =
    get (urb.urbitUrl ++ "/~/as/anon/~/auth.json") decodeAuthPayload


requestInitialAuth : Model msg b -> Cmd msg
requestInitialAuth model =
    Cmd.map model.toMsg <|
        Http.send InitAuthResponse (Http.get (model.urbitUrl ++ "/~/auth.json") decodeAuthPayload)


requestAuthAs : Model msg b -> String -> Cmd msg
requestAuthAs model shipName =
    Cmd.map model.toMsg <|
        Http.send AuthResponse (authGet model shipName)


requestAuthAsAnon : Model msg b -> Cmd msg
requestAuthAsAnon model =
    Cmd.map model.toMsg <|
        Http.send AnonAuthResponse (authGetAnon model)


receivePacket : List (Codec b) -> Result Http.Error String -> Msg b
receivePacket codecs resp =
    case resp of
        Err err ->
            PacketResponse <| Packet (Just (Err <| fromHttpError err))

        Ok str ->
            PacketResponse (pollDecode str codecs)


{-| Poke urbit ship
-}
sendPoke : Model msg b -> Poke -> Cmd msg
sendPoke urb poke =
    Cmd.map urb.toMsg <|
        Http.send PokeResponse
            (post (urb.urbitUrl ++ (pokeUrl poke))
                (Http.jsonBody (pokePayload urb.auth poke))
                decodePokePayload
            )


{-| Urbit subscription interface
-}
sendSub : Model msg b -> Subs -> SubsAction -> Cmd msg
sendSub urb sub act =
    let
        posturl =
            urb.urbitUrl ++ (subsUrl urb.auth sub act)
    in
        Cmd.map urb.toMsg <|
            Http.send SubsResponse
                (post posturl
                    (Http.jsonBody (subsPayload urb.auth sub))
                    decodeSubsPayload
                )


{-| Listen for Urbit response.
-}
poll : Model msg b -> Cmd msg
poll urb =
    Cmd.map urb.toMsg <|
        Http.send (receivePacket urb.codecs)
            (getString (urb.urbitUrl ++ (pollUrl urb.auth urb.eventId)) D.decodeString)


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
