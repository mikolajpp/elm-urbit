module Urb.Validator exposing (validPartsFromStr)

{-| Validation utilities. Since
Data between Urbit and frontend is passed using JSON,
we need abilitity to establish constraints on certain data structures,
such as ship names etc.

@docs validPartsFromStr

-}

import Http
import Json.Decode as D
import Regex exposing (find, regex)
import List exposing (..)
import Dict exposing (Dict)
import String.Interpolate exposing (interpolate)


-- Prefix syllabes


prefixSyllabe : String
prefixSyllabe =
    "dozmarbinwansamlitsighidfidlissogdirwacsabwissibrigsoldopmodfoglidhopdardorlorhodfolrintogsilmirholpaslacrovlivdalsatlibtabhanticpidtorbolfosdotlosdilforpilramtirwintadbicdifrocwidbisdasmidloprilnardapmolsanlocnovsitnidtipsicropwitnatpanminritpodmottamtolsavposnapnopsomfinfonbanmorworsipronnorbotwicsocwatdolmagpicdavbidbaltimtasmalligsivtagpadsaldivdactansidfabtarmonranniswolmispallasdismaprabtobrollatlonnodnavfignomnibpagsopralbilhaddocridmocpacravripfaltodtiltinhapmicfanpattaclabmogsimsonpinlomrictapfirhasbosbatpochactidhavsaplindibhosdabbitbarracparloddosbortochilmactomdigfilfasmithobharmighinradmashalraglagfadtopmophabnilnosmilfopfamdatnoldinhatnacrisfotribhocnimlarfitwalrapsarnalmoslandondanladdovrivbacpollaptalpitnambonrostonfodponsovnocsorlavmatmipfip"


suffixSyllabe : String
suffixSyllabe =
    "zodnecbudwessevpersutletfulpensytdurwepserwylsunrypsyxdyrnuphebpeglupdepdysputlughecryttyvsydnexlunmeplutseppesdelsulpedtemledtulmetwenbynhexfebpyldulhetmevruttylwydtepbesdexsefwycburderneppurrysrebdennutsubpetrulsynregtydsupsemwynrecmegnetsecmulnymtevwebsummutnyxrextebfushepbenmuswyxsymselrucdecwexsyrwetdylmynmesdetbetbeltuxtugmyrpelsyptermebsetdutdegtexsurfeltudnuxruxrenwytnubmedlytdusnebrumtynseglyxpunresredfunrevrefmectedrusbexlebduxrynnumpyxrygryxfeptyrtustyclegnemfermertenlusnussyltecmexpubrymtucfyllepdebbermughuttunbylsudpemdevlurdefbusbeprunmelpexdytbyttyplevmylwedducfurfexnulluclennerlexrupnedlecrydlydfenwelnydhusrelrudneshesfetdesretdunlernyrsebhulrylludremlysfynwerrycsugnysnyllyndyndemluxfedsedbecmunlyrtesmudnytbyrsenwegfyrmurtelreptegpecnelnevfes"


isValidSyllabe validstr str =
    let
        indexes =
            map .index (find (Regex.AtMost 1) (regex str) validstr)
    in
        case (head indexes) of
            Just x ->
                (x % 3) == 0

            Nothing ->
                False


isValidPrefix =
    isValidSyllabe prefixSyllabe


isValidSuffix =
    isValidSyllabe suffixSyllabe


validPart : String -> Result String String
validPart str =
    case String.length str of
        -- Single suffix
        3 ->
            Ok str

        -- Prefix + Suffix
        6 ->
            let
                prefix =
                    String.left 3 str

                suffix =
                    String.dropLeft 3 str
            in
                if
                    (isValidPrefix prefix)
                        && (isValidSuffix suffix)
                then
                    Ok str
                else
                    Err (interpolate "Part malformed: `{0}'" [ str ])

        _ ->
            Err (interpolate "Part has invalid length: `{0}'" [ str ])



-- Check that every 4 parts there was a separator (empty field)


isPartsFormatValid parts =
    let
        len =
            List.length parts

        seps =
            List.length (List.filter (\s -> String.length s == 0) parts)

        trueLen =
            len - seps
    in
        if trueLen < 4 then
            if seps > 0 then
                False
            else
                True
        else
            let
                whole =
                    case trueLen % 4 of
                        0 ->
                            1

                        _ ->
                            0
            in
                if
                    (trueLen // 4 - whole)
                        == seps
                then
                    True
                else
                    False


{-| Exctracts parts from Urbit address string
-}
validPartsFromStr : String -> Result (List String) (List String)
validPartsFromStr str =
    let
        parts =
            String.split "-" str

        vparts =
            List.map validPart (List.filter (\s -> String.length s > 0) parts)

        errors =
            List.map
                (\s ->
                    case s of
                        Err str ->
                            str

                        _ ->
                            ""
                )
                vparts
    in
        if (not (isPartsFormatValid parts)) then
            Err [ (interpolate "Incorrect parts format: `{0}'" [ str ]) ]
        else if (String.length (List.foldl (++) "" errors)) == 0 then
            Ok
                (List.map
                    (\p ->
                        case p of
                            Ok p ->
                                p

                            _ ->
                                ""
                    )
                    vparts
                )
        else
            Err errors
