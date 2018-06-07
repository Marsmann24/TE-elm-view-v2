module Decoderhelper exposing (..)

import Json.Decode exposing (Decoder, map, map2, map6, keyValuePairs, string)
import Dict exposing (Dict)
import Result exposing (Result(..))

int2 : Decoder Int
int2 =
    let convert : String -> Int
        convert a = Result.withDefault 0 (String.toInt a)
    in
    map convert string

float2 : Decoder Float
float2 =
    let convert : String -> Float
        convert a = Result.withDefault 0 (String.toFloat a)
    in
    map convert string

pseudolist : Decoder a -> Decoder (List a)
pseudolist decoder =
    map (List.map Tuple.second) (keyValuePairs decoder)

listheadwithdefault : a -> Decoder (List a) -> Decoder a
listheadwithdefault  default decoder =
    map (\x -> Maybe.withDefault default (List.head x)) decoder

intDictDecoder : a -> Decoder a -> Decoder (Dict Int a)
intDictDecoder default decoder =
    let makeInt : (String, a) -> (Int, a)
        makeInt (key, value) =
            case (String.toInt key) of
                Ok result ->
                    (result, value)
                _ ->
                    (-1, default)
    in
    map
        Dict.fromList
        (map
            (List.map makeInt)
            (keyValuePairs decoder)
        )

map12 : (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11-> b) -> Decoder a0 -> Decoder a1 -> Decoder a2 -> Decoder a3 -> Decoder a4 -> Decoder a5 -> Decoder a6 -> Decoder a7 -> Decoder a8 -> Decoder a9 -> Decoder a10 -> Decoder a11 -> Decoder b
map12 function decoder0 decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 decoder9 decoder10 decoder11 =
    (map2
        (\a b -> function a.a0 a.a1 a.a2 a.a3 a.a4 a.a5 b.b0 b.b1 b.b2 b.b3 b.b4 b.b5)
        (map6
            (\a0 a1 a2 a3 a4 a5 ->
                { a0 = a0
                , a1 = a1
                , a2 = a2
                , a3 = a3
                , a4 = a4
                , a5 = a5
                }
            )
            decoder0 decoder1 decoder2 decoder3 decoder4 decoder5
        )
        (map6
            (\b0 b1 b2 b3 b4 b5 ->
                { b0 = b0
                , b1 = b1
                , b2 = b2
                , b3 = b3
                , b4 = b4
                , b5 = b5
                }
            )
            decoder6 decoder7 decoder8 decoder9 decoder10 decoder11
        )
    )
