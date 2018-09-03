module Urb.Error exposing (emptyError, ErrResponse, ErrPayload, fromHttpError)

{-| Urbit API error handling
You need to use this module in order to be able
to handle Urbit API calls errors

@docs emptyError, ErrResponse, fromHttpError, ErrPayload

-}

import Http
import Json.Decode as D
import Json.Encode as E
import Regex
import List exposing (..)
import Dict exposing (Dict)
import Debug exposing (toString)


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
    { desc : String
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
            { desc = "Urb request has timed out", payload = Nothing }

        Http.NetworkError ->
            { desc = "Network error has occured. Check your connectivity.", payload = Nothing }

        Http.BadStatus resp ->
            { desc = "Urb bad status received: " ++ (toString resp.status.code), payload = errPayloadFromResp resp }

        Http.BadUrl resp ->
            { desc = "Bad URL, unable to perform request", payload = Nothing }

        Http.BadPayload _ resp ->
            let
                authErr =
                    (D.decodeString decodeErrAuth resp.body)
            in
                case authErr of
                    Ok authResp ->
                        if authResp.ok then
                            { desc = "Redirection auth not supported.", payload = Nothing }
                        else
                            { desc = "Authentication failed. Please login.", payload = Nothing }

                    Err er ->
                        { desc = "Urb received malformed response: " ++ (toString er), payload = errPayloadFromResp resp }
