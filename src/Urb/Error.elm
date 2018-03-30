module Urb.Error exposing (emptyError, ErrResponse, fromHttpError)

{-| Urbit API error handling
You need to use this module in order to be able
to handle Urbit API calls errors

@docs emptyError, ErrResponse, fromHttpError

-}

import Http
import Json.Decode as D
import Json.Encode as E
import Regex exposing (find, regex)
import List exposing (..)
import Dict exposing (Dict)


{-| Urbit error payload.
-}
type alias ErrPayload =
    { coup : String
    , mess : String
    }


type alias ErrAuth =
    { ok : Bool
    , redir : String
    }


{-| Error response.
error - description of error
payload - @ErrPayload
-}
type alias ErrResponse =
    { error : String
    , payload : Maybe ErrPayload
    }


{-| Empty error payload.
-}
emptyError : ErrPayload
emptyError =
    { coup = "", mess = "" }


decodeErrPayload : D.Decoder ErrPayload
decodeErrPayload =
    D.map2 ErrPayload
        (D.field "fail" D.string)
        (D.field "mess" D.string)


errPayloadFromResp resp =
    case (D.decodeString decodeErrPayload resp.body) of
        Ok errPayload ->
            Just errPayload

        Err err ->
            Nothing


decodeErrAuth : D.Decoder ErrAuth
decodeErrAuth =
    D.map2 ErrAuth
        (D.field "ok" D.bool)
        (D.field "red" D.string)


{-| Translate Http.Error Urbit response into @ErrResponse
-}
fromHttpError : Http.Error -> ErrResponse
fromHttpError err =
    case err of
        Http.Timeout ->
            { error = "Urb request has timed out", payload = Nothing }

        Http.NetworkError ->
            { error = "Network error has occured. Check your connectivity.", payload = Nothing }

        Http.BadStatus resp ->
            { error = "Urb bad status received: " ++ (toString resp.status.code), payload = errPayloadFromResp resp }

        Http.BadUrl resp ->
            { error = "Bad URL, unable to perform request", payload = Nothing }

        Http.BadPayload err resp ->
            let
                authErr =
                    (D.decodeString decodeErrAuth resp.body)
            in
                case authErr of
                    Ok authResp ->
                        if authResp.ok then
                            { error = "Redirection auth not supported.", payload = Nothing }
                        else
                            { error = "Authentication failed. Please login.", payload = Nothing }

                    Err err ->
                        { error = "Urb received malformed response: " ++ err, payload = errPayloadFromResp resp }
