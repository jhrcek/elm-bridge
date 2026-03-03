module MyTests exposing (..)
-- This module requires the following packages:
-- * bartavelle/json-helpers
-- * NoRedInk/elm-json-decode-pipeline
-- * elm/json
-- * elm-explorations/test

import Dict exposing (Dict, fromList)
import Expect exposing (Expectation, equal)
import Set exposing (Set)
import Json.Decode exposing (field, Value)
import Json.Encode
import Json.Helpers exposing (..)
import String
import Test exposing (Test, describe, test)

newtypeDecode : Test
newtypeDecode = describe "Newtype decoding checks"
              [ ntDecode1
              , ntDecode2
              , ntDecode3
              , ntDecode4
              ]

newtypeEncode : Test
newtypeEncode = describe "Newtype encoding checks"
              [ ntEncode1
              , ntEncode2
              , ntEncode3
              , ntEncode4
              ]

recordDecode : Test
recordDecode = describe "Record decoding checks"
              [ recordDecode1
              , recordDecode2
              , recordDecodeNestTuple
              ]

recordEncode : Test
recordEncode = describe "Record encoding checks"
              [ recordEncode1
              , recordEncode2
              , recordEncodeNestTuple
              ]

sumDecode : Test
sumDecode = describe "Sum decoding checks"
              [ sumDecode01
              , sumDecode02
              , sumDecode03
              , sumDecode04
              , sumDecode05
              , sumDecode06
              , sumDecode07
              , sumDecode08
              , sumDecode09
              , sumDecode10
              , sumDecode11
              , sumDecode12
              , sumDecodeUntagged
              , sumDecodeIncludeUnit
              ]

sumEncode : Test
sumEncode = describe "Sum encoding checks"
              [ sumEncode01
              , sumEncode02
              , sumEncode03
              , sumEncode04
              , sumEncode05
              , sumEncode06
              , sumEncode07
              , sumEncode08
              , sumEncode09
              , sumEncode10
              , sumEncode11
              , sumEncode12
              , sumEncodeUntagged
              , sumEncodeIncludeUnit
              ]

simpleDecode : Test
simpleDecode = describe "Simple records/types decode checks"
                [ simpleDecode01
                , simpleDecode02
                , simpleDecode03
                , simpleDecode04
                , simplerecordDecode01
                , simplerecordDecode02
                , simplerecordDecode03
                , simplerecordDecode04
                ]

simpleEncode : Test
simpleEncode = describe "Simple records/types encode checks"
                [ simpleEncode01
                , simpleEncode02
                , simpleEncode03
                , simpleEncode04
                , simplerecordEncode01
                , simplerecordEncode02
                , simplerecordEncode03
                , simplerecordEncode04
                ]

-- this is done to prevent artificial differences due to object ordering, this won't work with Maybe's though :(
equalHack : String -> String -> Expectation
equalHack a b =
    let remix = Json.Decode.decodeString Json.Decode.value
    in equal (remix a) (remix b)


type Record1 a = Record1
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   , jmap: (Dict String Int)
   }

jsonDecRecord1 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record1 a )
jsonDecRecord1 localDecoder_a =
   Json.Decode.succeed (\pfoo pbar pbaz pqux pjmap -> (Record1 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux, jmap = pjmap}))
   |> required "foo" (Json.Decode.int)
   |> fnullable "bar" (Json.Decode.int)
   |> required "baz" (localDecoder_a)
   |> fnullable "qux" (localDecoder_a)
   |> required "jmap" (Json.Decode.dict (Json.Decode.int))

jsonEncRecord1 : (a -> Value) -> Record1 a -> Value
jsonEncRecord1 localEncoder_a (Record1 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   , ("jmap", (Json.Encode.dict identity (Json.Encode.int)) val.jmap)
   ]



type Record2 a = Record2
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   }

jsonDecRecord2 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record2 a )
jsonDecRecord2 localDecoder_a =
   Json.Decode.succeed (\pfoo pbar pbaz pqux -> (Record2 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux}))
   |> required "foo" (Json.Decode.int)
   |> fnullable "bar" (Json.Decode.int)
   |> required "baz" (localDecoder_a)
   |> fnullable "qux" (localDecoder_a)

jsonEncRecord2 : (a -> Value) -> Record2 a -> Value
jsonEncRecord2 localEncoder_a (Record2 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   ]



type RecordNestTuple a =
    RecordNestTuple (a, (a, a))

jsonDecRecordNestTuple : Json.Decode.Decoder a -> Json.Decode.Decoder ( RecordNestTuple a )
jsonDecRecordNestTuple localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map RecordNestTuple (Json.Decode.map2 tuple2 (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (Json.Decode.map2 tuple2 (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))))


jsonEncRecordNestTuple : (a -> Value) -> RecordNestTuple a -> Value
jsonEncRecordNestTuple localEncoder_a(RecordNestTuple v1) =
    (\(t1,t2) -> Json.Encode.list identity [(localEncoder_a) t1,((\(t3,t4) -> Json.Encode.list identity [(localEncoder_a) t3,(localEncoder_a) t4])) t2]) v1



type Sum01 a =
    Sum01A a
    | Sum01B (Maybe a)
    | Sum01C a a
    | Sum01D {foo: a}
    | Sum01E {bar: Int, baz: Int}

jsonDecSum01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum01 a )
jsonDecSum01 localDecoder_a =
    let jsonDecDictSum01 = Dict.fromList
            [ ("Sum01A", Json.Decode.lazy (\_ -> Json.Decode.map Sum01A (localDecoder_a)))
            , ("Sum01B", Json.Decode.lazy (\_ -> Json.Decode.map Sum01B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum01C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum01C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum01D", Json.Decode.lazy (\_ -> Json.Decode.map Sum01D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum01E", Json.Decode.lazy (\_ -> Json.Decode.map Sum01E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum01 = Set.fromList ["Sum01D", "Sum01E"]
    in  decodeSumTaggedObject "Sum01" "tag" "content" jsonDecDictSum01 jsonDecObjectSetSum01

jsonEncSum01 : (a -> Value) -> Sum01 a -> Value
jsonEncSum01 localEncoder_a val =
    let keyval v = case v of
                    Sum01A v1 -> ("Sum01A", encodeValue (localEncoder_a v1))
                    Sum01B v1 -> ("Sum01B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum01C v1 v2 -> ("Sum01C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum01D vs -> ("Sum01D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum01E vs -> ("Sum01E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum02 a =
    Sum02A a
    | Sum02B (Maybe a)
    | Sum02C a a
    | Sum02D {foo: a}
    | Sum02E {bar: Int, baz: Int}

jsonDecSum02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum02 a )
jsonDecSum02 localDecoder_a =
    let jsonDecDictSum02 = Dict.fromList
            [ ("Sum02A", Json.Decode.lazy (\_ -> Json.Decode.map Sum02A (localDecoder_a)))
            , ("Sum02B", Json.Decode.lazy (\_ -> Json.Decode.map Sum02B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum02C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum02C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum02D", Json.Decode.lazy (\_ -> Json.Decode.map Sum02D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum02E", Json.Decode.lazy (\_ -> Json.Decode.map Sum02E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum02 = Set.fromList ["Sum02D", "Sum02E"]
    in  decodeSumTaggedObject "Sum02" "tag" "content" jsonDecDictSum02 jsonDecObjectSetSum02

jsonEncSum02 : (a -> Value) -> Sum02 a -> Value
jsonEncSum02 localEncoder_a val =
    let keyval v = case v of
                    Sum02A v1 -> ("Sum02A", encodeValue (localEncoder_a v1))
                    Sum02B v1 -> ("Sum02B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum02C v1 v2 -> ("Sum02C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum02D vs -> ("Sum02D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum02E vs -> ("Sum02E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum03 a =
    Sum03A a
    | Sum03B (Maybe a)
    | Sum03C a a
    | Sum03D {foo: a}
    | Sum03E {bar: Int, baz: Int}

jsonDecSum03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum03 a )
jsonDecSum03 localDecoder_a =
    let jsonDecDictSum03 = Dict.fromList
            [ ("Sum03A", Json.Decode.lazy (\_ -> Json.Decode.map Sum03A (localDecoder_a)))
            , ("Sum03B", Json.Decode.lazy (\_ -> Json.Decode.map Sum03B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum03C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum03C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum03D", Json.Decode.lazy (\_ -> Json.Decode.map Sum03D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum03E", Json.Decode.lazy (\_ -> Json.Decode.map Sum03E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum03 = Set.fromList ["Sum03D", "Sum03E"]
    in  decodeSumTaggedObject "Sum03" "tag" "content" jsonDecDictSum03 jsonDecObjectSetSum03

jsonEncSum03 : (a -> Value) -> Sum03 a -> Value
jsonEncSum03 localEncoder_a val =
    let keyval v = case v of
                    Sum03A v1 -> ("Sum03A", encodeValue (localEncoder_a v1))
                    Sum03B v1 -> ("Sum03B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum03C v1 v2 -> ("Sum03C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum03D vs -> ("Sum03D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum03E vs -> ("Sum03E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum04 a =
    Sum04A a
    | Sum04B (Maybe a)
    | Sum04C a a
    | Sum04D {foo: a}
    | Sum04E {bar: Int, baz: Int}

jsonDecSum04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum04 a )
jsonDecSum04 localDecoder_a =
    let jsonDecDictSum04 = Dict.fromList
            [ ("Sum04A", Json.Decode.lazy (\_ -> Json.Decode.map Sum04A (localDecoder_a)))
            , ("Sum04B", Json.Decode.lazy (\_ -> Json.Decode.map Sum04B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum04C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum04C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum04D", Json.Decode.lazy (\_ -> Json.Decode.map Sum04D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum04E", Json.Decode.lazy (\_ -> Json.Decode.map Sum04E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum04 = Set.fromList ["Sum04D", "Sum04E"]
    in  decodeSumTaggedObject "Sum04" "tag" "content" jsonDecDictSum04 jsonDecObjectSetSum04

jsonEncSum04 : (a -> Value) -> Sum04 a -> Value
jsonEncSum04 localEncoder_a val =
    let keyval v = case v of
                    Sum04A v1 -> ("Sum04A", encodeValue (localEncoder_a v1))
                    Sum04B v1 -> ("Sum04B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum04C v1 v2 -> ("Sum04C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum04D vs -> ("Sum04D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum04E vs -> ("Sum04E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum05 a =
    Sum05A a
    | Sum05B (Maybe a)
    | Sum05C a a
    | Sum05D {foo: a}
    | Sum05E {bar: Int, baz: Int}

jsonDecSum05 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum05 a )
jsonDecSum05 localDecoder_a =
    let jsonDecDictSum05 = Dict.fromList
            [ ("Sum05A", Json.Decode.lazy (\_ -> Json.Decode.map Sum05A (localDecoder_a)))
            , ("Sum05B", Json.Decode.lazy (\_ -> Json.Decode.map Sum05B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum05C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum05C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum05D", Json.Decode.lazy (\_ -> Json.Decode.map Sum05D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum05E", Json.Decode.lazy (\_ -> Json.Decode.map Sum05E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum05" jsonDecDictSum05

jsonEncSum05 : (a -> Value) -> Sum05 a -> Value
jsonEncSum05 localEncoder_a val =
    let keyval v = case v of
                    Sum05A v1 -> ("Sum05A", encodeValue (localEncoder_a v1))
                    Sum05B v1 -> ("Sum05B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum05C v1 v2 -> ("Sum05C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum05D vs -> ("Sum05D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum05E vs -> ("Sum05E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum06 a =
    Sum06A a
    | Sum06B (Maybe a)
    | Sum06C a a
    | Sum06D {foo: a}
    | Sum06E {bar: Int, baz: Int}

jsonDecSum06 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum06 a )
jsonDecSum06 localDecoder_a =
    let jsonDecDictSum06 = Dict.fromList
            [ ("Sum06A", Json.Decode.lazy (\_ -> Json.Decode.map Sum06A (localDecoder_a)))
            , ("Sum06B", Json.Decode.lazy (\_ -> Json.Decode.map Sum06B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum06C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum06C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum06D", Json.Decode.lazy (\_ -> Json.Decode.map Sum06D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum06E", Json.Decode.lazy (\_ -> Json.Decode.map Sum06E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum06" jsonDecDictSum06

jsonEncSum06 : (a -> Value) -> Sum06 a -> Value
jsonEncSum06 localEncoder_a val =
    let keyval v = case v of
                    Sum06A v1 -> ("Sum06A", encodeValue (localEncoder_a v1))
                    Sum06B v1 -> ("Sum06B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum06C v1 v2 -> ("Sum06C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum06D vs -> ("Sum06D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum06E vs -> ("Sum06E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum07 a =
    Sum07A a
    | Sum07B (Maybe a)
    | Sum07C a a
    | Sum07D {foo: a}
    | Sum07E {bar: Int, baz: Int}

jsonDecSum07 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum07 a )
jsonDecSum07 localDecoder_a =
    let jsonDecDictSum07 = Dict.fromList
            [ ("Sum07A", Json.Decode.lazy (\_ -> Json.Decode.map Sum07A (localDecoder_a)))
            , ("Sum07B", Json.Decode.lazy (\_ -> Json.Decode.map Sum07B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum07C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum07C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum07D", Json.Decode.lazy (\_ -> Json.Decode.map Sum07D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum07E", Json.Decode.lazy (\_ -> Json.Decode.map Sum07E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum07" jsonDecDictSum07

jsonEncSum07 : (a -> Value) -> Sum07 a -> Value
jsonEncSum07 localEncoder_a val =
    let keyval v = case v of
                    Sum07A v1 -> ("Sum07A", encodeValue (localEncoder_a v1))
                    Sum07B v1 -> ("Sum07B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum07C v1 v2 -> ("Sum07C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum07D vs -> ("Sum07D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum07E vs -> ("Sum07E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum08 a =
    Sum08A a
    | Sum08B (Maybe a)
    | Sum08C a a
    | Sum08D {foo: a}
    | Sum08E {bar: Int, baz: Int}

jsonDecSum08 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum08 a )
jsonDecSum08 localDecoder_a =
    let jsonDecDictSum08 = Dict.fromList
            [ ("Sum08A", Json.Decode.lazy (\_ -> Json.Decode.map Sum08A (localDecoder_a)))
            , ("Sum08B", Json.Decode.lazy (\_ -> Json.Decode.map Sum08B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum08C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum08C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum08D", Json.Decode.lazy (\_ -> Json.Decode.map Sum08D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum08E", Json.Decode.lazy (\_ -> Json.Decode.map Sum08E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum08" jsonDecDictSum08

jsonEncSum08 : (a -> Value) -> Sum08 a -> Value
jsonEncSum08 localEncoder_a val =
    let keyval v = case v of
                    Sum08A v1 -> ("Sum08A", encodeValue (localEncoder_a v1))
                    Sum08B v1 -> ("Sum08B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum08C v1 v2 -> ("Sum08C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum08D vs -> ("Sum08D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum08E vs -> ("Sum08E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum09 a =
    Sum09A a
    | Sum09B (Maybe a)
    | Sum09C a a
    | Sum09D {foo: a}
    | Sum09E {bar: Int, baz: Int}

jsonDecSum09 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum09 a )
jsonDecSum09 localDecoder_a =
    let jsonDecDictSum09 = Dict.fromList
            [ ("Sum09A", Json.Decode.lazy (\_ -> Json.Decode.map Sum09A (localDecoder_a)))
            , ("Sum09B", Json.Decode.lazy (\_ -> Json.Decode.map Sum09B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum09C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum09C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum09D", Json.Decode.lazy (\_ -> Json.Decode.map Sum09D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum09E", Json.Decode.lazy (\_ -> Json.Decode.map Sum09E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum09" jsonDecDictSum09

jsonEncSum09 : (a -> Value) -> Sum09 a -> Value
jsonEncSum09 localEncoder_a val =
    let keyval v = case v of
                    Sum09A v1 -> ("Sum09A", encodeValue (localEncoder_a v1))
                    Sum09B v1 -> ("Sum09B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum09C v1 v2 -> ("Sum09C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum09D vs -> ("Sum09D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum09E vs -> ("Sum09E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum10 a =
    Sum10A a
    | Sum10B (Maybe a)
    | Sum10C a a
    | Sum10D {foo: a}
    | Sum10E {bar: Int, baz: Int}

jsonDecSum10 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum10 a )
jsonDecSum10 localDecoder_a =
    let jsonDecDictSum10 = Dict.fromList
            [ ("Sum10A", Json.Decode.lazy (\_ -> Json.Decode.map Sum10A (localDecoder_a)))
            , ("Sum10B", Json.Decode.lazy (\_ -> Json.Decode.map Sum10B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum10C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum10C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum10D", Json.Decode.lazy (\_ -> Json.Decode.map Sum10D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum10E", Json.Decode.lazy (\_ -> Json.Decode.map Sum10E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum10" jsonDecDictSum10

jsonEncSum10 : (a -> Value) -> Sum10 a -> Value
jsonEncSum10 localEncoder_a val =
    let keyval v = case v of
                    Sum10A v1 -> ("Sum10A", encodeValue (localEncoder_a v1))
                    Sum10B v1 -> ("Sum10B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum10C v1 v2 -> ("Sum10C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum10D vs -> ("Sum10D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum10E vs -> ("Sum10E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum11 a =
    Sum11A a
    | Sum11B (Maybe a)
    | Sum11C a a
    | Sum11D {foo: a}
    | Sum11E {bar: Int, baz: Int}

jsonDecSum11 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum11 a )
jsonDecSum11 localDecoder_a =
    let jsonDecDictSum11 = Dict.fromList
            [ ("Sum11A", Json.Decode.lazy (\_ -> Json.Decode.map Sum11A (localDecoder_a)))
            , ("Sum11B", Json.Decode.lazy (\_ -> Json.Decode.map Sum11B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum11C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum11C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum11D", Json.Decode.lazy (\_ -> Json.Decode.map Sum11D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum11E", Json.Decode.lazy (\_ -> Json.Decode.map Sum11E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum11" jsonDecDictSum11

jsonEncSum11 : (a -> Value) -> Sum11 a -> Value
jsonEncSum11 localEncoder_a val =
    let keyval v = case v of
                    Sum11A v1 -> ("Sum11A", encodeValue (localEncoder_a v1))
                    Sum11B v1 -> ("Sum11B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum11C v1 v2 -> ("Sum11C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum11D vs -> ("Sum11D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum11E vs -> ("Sum11E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum12 a =
    Sum12A a
    | Sum12B (Maybe a)
    | Sum12C a a
    | Sum12D {foo: a}
    | Sum12E {bar: Int, baz: Int}

jsonDecSum12 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum12 a )
jsonDecSum12 localDecoder_a =
    let jsonDecDictSum12 = Dict.fromList
            [ ("Sum12A", Json.Decode.lazy (\_ -> Json.Decode.map Sum12A (localDecoder_a)))
            , ("Sum12B", Json.Decode.lazy (\_ -> Json.Decode.map Sum12B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum12C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum12C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum12D", Json.Decode.lazy (\_ -> Json.Decode.map Sum12D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum12E", Json.Decode.lazy (\_ -> Json.Decode.map Sum12E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum12" jsonDecDictSum12

jsonEncSum12 : (a -> Value) -> Sum12 a -> Value
jsonEncSum12 localEncoder_a val =
    let keyval v = case v of
                    Sum12A v1 -> ("Sum12A", encodeValue (localEncoder_a v1))
                    Sum12B v1 -> ("Sum12B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum12C v1 v2 -> ("Sum12C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum12D vs -> ("Sum12D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum12E vs -> ("Sum12E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Simple01 a =
    Simple01 a

jsonDecSimple01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple01 a )
jsonDecSimple01 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple01 (localDecoder_a))


jsonEncSimple01 : (a -> Value) -> Simple01 a -> Value
jsonEncSimple01 localEncoder_a(Simple01 v1) =
    localEncoder_a v1



type Simple02 a =
    Simple02 a

jsonDecSimple02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple02 a )
jsonDecSimple02 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple02 (localDecoder_a))


jsonEncSimple02 : (a -> Value) -> Simple02 a -> Value
jsonEncSimple02 localEncoder_a(Simple02 v1) =
    localEncoder_a v1



type Simple03 a =
    Simple03 a

jsonDecSimple03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple03 a )
jsonDecSimple03 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple03 (localDecoder_a))


jsonEncSimple03 : (a -> Value) -> Simple03 a -> Value
jsonEncSimple03 localEncoder_a(Simple03 v1) =
    localEncoder_a v1



type Simple04 a =
    Simple04 a

jsonDecSimple04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple04 a )
jsonDecSimple04 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple04 (localDecoder_a))


jsonEncSimple04 : (a -> Value) -> Simple04 a -> Value
jsonEncSimple04 localEncoder_a(Simple04 v1) =
    localEncoder_a v1



type SimpleRecord01 a = SimpleRecord01
   { qux: a
   }

jsonDecSimpleRecord01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord01 a )
jsonDecSimpleRecord01 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord01 {qux = pqux}))
   |> required "qux" (localDecoder_a)

jsonEncSimpleRecord01 : (a -> Value) -> SimpleRecord01 a -> Value
jsonEncSimpleRecord01 localEncoder_a (SimpleRecord01 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord02 a = SimpleRecord02
   { qux: a
   }

jsonDecSimpleRecord02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord02 a )
jsonDecSimpleRecord02 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord02 {qux = pqux})) |> custom (localDecoder_a)

jsonEncSimpleRecord02 : (a -> Value) -> SimpleRecord02 a -> Value
jsonEncSimpleRecord02 localEncoder_a (SimpleRecord02 val) =
   localEncoder_a val.qux


type SimpleRecord03 a = SimpleRecord03
   { qux: a
   }

jsonDecSimpleRecord03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord03 a )
jsonDecSimpleRecord03 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord03 {qux = pqux}))
   |> required "qux" (localDecoder_a)

jsonEncSimpleRecord03 : (a -> Value) -> SimpleRecord03 a -> Value
jsonEncSimpleRecord03 localEncoder_a (SimpleRecord03 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord04 a = SimpleRecord04
   { qux: a
   }

jsonDecSimpleRecord04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord04 a )
jsonDecSimpleRecord04 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord04 {qux = pqux})) |> custom (localDecoder_a)

jsonEncSimpleRecord04 : (a -> Value) -> SimpleRecord04 a -> Value
jsonEncSimpleRecord04 localEncoder_a (SimpleRecord04 val) =
   localEncoder_a val.qux


type SumUntagged a =
    SMInt Int
    | SMList a

jsonDecSumUntagged : Json.Decode.Decoder a -> Json.Decode.Decoder ( SumUntagged a )
jsonDecSumUntagged localDecoder_a =
    let jsonDecDictSumUntagged = Dict.fromList
            [ ("SMInt", Json.Decode.lazy (\_ -> Json.Decode.map SMInt (Json.Decode.int)))
            , ("SMList", Json.Decode.lazy (\_ -> Json.Decode.map SMList (localDecoder_a)))
            ]
    in  Json.Decode.oneOf (Dict.values jsonDecDictSumUntagged)

jsonEncSumUntagged : (a -> Value) -> SumUntagged a -> Value
jsonEncSumUntagged localEncoder_a val =
    let keyval v = case v of
                    SMInt v1 -> ("SMInt", encodeValue (Json.Encode.int v1))
                    SMList v1 -> ("SMList", encodeValue (localEncoder_a v1))
    in encodeSumUntagged keyval val



type SumIncludeUnit a =
    SumIncludeUnitZero 
    | SumIncludeUnitOne a
    | SumIncludeUnitTwo a a

jsonDecSumIncludeUnit : Json.Decode.Decoder a -> Json.Decode.Decoder ( SumIncludeUnit a )
jsonDecSumIncludeUnit localDecoder_a =
    let jsonDecDictSumIncludeUnit = Dict.fromList
            [ ("SumIncludeUnitZero", Json.Decode.lazy (\_ -> Json.Decode.succeed SumIncludeUnitZero))
            , ("SumIncludeUnitOne", Json.Decode.lazy (\_ -> Json.Decode.map SumIncludeUnitOne (localDecoder_a)))
            , ("SumIncludeUnitTwo", Json.Decode.lazy (\_ -> Json.Decode.map2 SumIncludeUnitTwo (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            ]
        jsonDecObjectSetSumIncludeUnit = Set.fromList ["SumIncludeUnitZero"]
    in  decodeSumTaggedObject "SumIncludeUnit" "tag" "content" jsonDecDictSumIncludeUnit jsonDecObjectSetSumIncludeUnit

jsonEncSumIncludeUnit : (a -> Value) -> SumIncludeUnit a -> Value
jsonEncSumIncludeUnit localEncoder_a val =
    let keyval v = case v of
                    SumIncludeUnitZero  -> ("SumIncludeUnitZero", encodeValue (Json.Encode.list identity []))
                    SumIncludeUnitOne v1 -> ("SumIncludeUnitOne", encodeValue (localEncoder_a v1))
                    SumIncludeUnitTwo v1 v2 -> ("SumIncludeUnitTwo", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
    in encodeSumTaggedObject "tag" "content" keyval val



type alias NT1  = (List Int)

jsonDecNT1 : Json.Decode.Decoder ( NT1 )
jsonDecNT1 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT1 : NT1 -> Value
jsonEncNT1  val = (Json.Encode.list Json.Encode.int) val



type alias NT2  = (List Int)

jsonDecNT2 : Json.Decode.Decoder ( NT2 )
jsonDecNT2 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT2 : NT2 -> Value
jsonEncNT2  val = (Json.Encode.list Json.Encode.int) val



type alias NT3  = (List Int)

jsonDecNT3 : Json.Decode.Decoder ( NT3 )
jsonDecNT3 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT3 : NT3 -> Value
jsonEncNT3  val = (Json.Encode.list Json.Encode.int) val



type NT4  = NT4
   { foo: (List Int)
   }

jsonDecNT4 : Json.Decode.Decoder ( NT4 )
jsonDecNT4 =
   Json.Decode.succeed (\pfoo -> (NT4 {foo = pfoo}))
   |> required "foo" (Json.Decode.list (Json.Decode.int))

jsonEncNT4 : NT4 -> Value
jsonEncNT4  (NT4 val) =
   Json.Encode.object
   [ ("foo", (Json.Encode.list Json.Encode.int) val.foo)
   ]



sumEncode01 : Test
sumEncode01 = describe "Sum encode 01"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01D {foo = []}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":-1,\"baz\":-4}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = -1, baz = -4}))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[1,-4,1]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [1,-4,1])))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":-6,\"baz\":-2}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = -6, baz = -2}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[5,1,-5,10,-2,9]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [5,1,-5,10,-2,9]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-8,12,-11,-7,-12,-11,9,2,6,-7,2,11]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [-8,12,-11,-7,-12,-11,9,2,6,-7,2,11]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-11,-4,10,14,1,-8,-4,7,2,-2,2,-6,-14,3]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [-11,-4,10,14,1,-8,-4,7,2,-2,2,-6,-14,3]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A []))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[-13,13,-4,-13,5,-17,3,-18,12,9,1,13]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [-13,13,-4,-13,5,-17,3,-18,12,9,1,13])))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[18,17,12,19,18,8,-15,-19,-18,-18,-11,-18,19,9,7],[-19,-19,-19,5,-6,17,-9,-12,-17,-9,11,-1,9,-13,0]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [18,17,12,19,18,8,-15,-19,-18,-18,-11,-18,19,9,7] [-19,-19,-19,5,-6,17,-9,-12,-17,-9,11,-1,9,-13,0]))))
  ]

sumEncode02 : Test
sumEncode02 = describe "Sum encode 02"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":-1,\"baz\":2}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = -1, baz = 2}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [])))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[3,6,-6,-4,0],[-3,4,-4]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02C [3,6,-6,-4,0] [-3,4,-4]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":-6,\"baz\":-8}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = -6, baz = -8}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[7,-4]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02A [7,-4]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[-7,-10,8,2,-2,3,-11]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [-7,-10,8,2,-2,3,-11])))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[-2,12,12,6,14,5,-14,1]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02A [-2,12,12,6,14,5,-14,1]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":4,\"baz\":2}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 4, baz = 2}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":14,\"baz\":17}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 14, baz = 17}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[-19,-1,-9,-20,-7,-4,-14,1,17,14,7,12,18,-10],[9,-15,13,-5,13,-12,8,16]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02C [-19,-1,-9,-20,-7,-4,-14,1,17,14,7,12,18,-10] [9,-15,13,-5,13,-12,8,16]))))
  ]

sumEncode03 : Test
sumEncode03 = describe "Sum encode 03"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":0,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":-2,\"baz\":-2}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = -2, baz = -2}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[-4,4,-3]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A [-4,4,-3]))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":-3,\"baz\":2}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = -3, baz = 2}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03B (Just [])))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":3,\"baz\":-8}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 3, baz = -8}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":-8,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = -8, baz = 0}))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[-7,-2,2,-4,-5,-1,4]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03B (Just [-7,-2,2,-4,-5,-1,4])))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":8,\"baz\":-15}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 8, baz = -15}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[10,-8,-16,6,15,-1,13,-15,-6,2,-3,8]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A [10,-8,-16,6,15,-1,13,-15,-6,2,-3,8]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[13,1,-6,-3,18,3,18,-18,10,-18,-16,14,15,19,-13,-10],[-2,-17,-17,10,-2,5,-17]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [13,1,-6,-3,18,3,18,-18,10,-18,-16,14,15,19,-13,-10] [-2,-17,-17,10,-2,5,-17]))))
  ]

sumEncode04 : Test
sumEncode04 = describe "Sum encode 04"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[2,1],[-2]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [2,1] [-2]))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[-3]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [-3])))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[2,5,5,4]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [2,5,5,4]}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[0,-3,-3,2,-2,-1],[8,0]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [0,-3,-3,2,-2,-1] [8,0]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[-5,6,7,-2,3,5,10,-6]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [-5,6,7,-2,3,5,10,-6]}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[-6]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [-6])))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[11,3,11,-6,-3,-4,10,5]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [11,3,11,-6,-3,-4,10,5]}))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[9,16,-15,7,14,15,12,8,-2,-10,10,-12]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [9,16,-15,7,14,15,12,8,-2,-10,10,-12]}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum04E\",\"bar\":-11,\"baz\":6}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04E {bar = -11, baz = 6}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = []}))))
  ]

sumEncode05 : Test
sumEncode05 = describe "Sum encode 05"
  [ test "1" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"Sum05A\":[]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A []))))
  , test "3" (\_ -> equalHack "{\"Sum05C\":[[4,-3,-3,2],[-2]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [4,-3,-3,2] [-2]))))
  , test "4" (\_ -> equalHack "{\"Sum05B\":[-6,0,2,-1,0]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [-6,0,2,-1,0])))))
  , test "5" (\_ -> equalHack "{\"Sum05E\":{\"bar\":4,\"baz\":8}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05E {bar = 4, baz = 8}))))
  , test "6" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[1,-7,8,8,-5,-9,10,-7]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = [1,-7,8,8,-5,-9,10,-7]}))))
  , test "7" (\_ -> equalHack "{\"Sum05C\":[[8,8,-11,-2,-10,-10,9,-8,-10,9],[9,-10,12,10]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [8,8,-11,-2,-10,-10,9,-8,-10,9] [9,-10,12,10]))))
  , test "8" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-14,\"baz\":1}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05E {bar = -14, baz = 1}))))
  , test "9" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[-6,12,1,6,-3,2,-3,2,-16,9,-14,-5]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = [-6,12,1,6,-3,2,-3,2,-16,9,-14,-5]}))))
  , test "10" (\_ -> equalHack "{\"Sum05B\":[-16,-1,-4,3,-17,-11,12,8,10,12,-17]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [-16,-1,-4,3,-17,-11,12,8,10,12,-17])))))
  , test "11" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-3,\"baz\":8}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05E {bar = -3, baz = 8}))))
  ]

sumEncode06 : Test
sumEncode06 = describe "Sum encode 06"
  [ test "1" (\_ -> equalHack "{\"Sum06C\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [] []))))
  , test "2" (\_ -> equalHack "{\"Sum06C\":[[-1],[]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [-1] []))))
  , test "3" (\_ -> equalHack "{\"Sum06C\":[[-1,-1,0,-1],[2,4]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [-1,-1,0,-1] [2,4]))))
  , test "4" (\_ -> equalHack "{\"Sum06B\":[4,0,-3,2]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [4,0,-3,2])))))
  , test "5" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[-7,8,1,0,4,4,-2]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [-7,8,1,0,4,4,-2]}))))
  , test "6" (\_ -> equalHack "{\"Sum06A\":[-7,0,10,-9,6,-7,-8,3]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06A [-7,0,10,-9,6,-7,-8,3]))))
  , test "7" (\_ -> equalHack "{\"Sum06C\":[[-10,11],[6,2,7,8,3]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [-10,11] [6,2,7,8,3]))))
  , test "8" (\_ -> equalHack "{\"Sum06E\":{\"bar\":-3,\"baz\":-6}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = -3, baz = -6}))))
  , test "9" (\_ -> equalHack "{\"Sum06E\":{\"bar\":-14,\"baz\":7}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = -14, baz = 7}))))
  , test "10" (\_ -> equalHack "{\"Sum06C\":[[-12],[10,18,-13]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [-12] [10,18,-13]))))
  , test "11" (\_ -> equalHack "{\"Sum06A\":[15,-13,9,-3]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06A [15,-13,9,-3]))))
  ]

sumEncode07 : Test
sumEncode07 = describe "Sum encode 07"
  [ test "1" (\_ -> equalHack "{\"Sum07A\":[]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A []))))
  , test "2" (\_ -> equalHack "{\"Sum07B\":[-2]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [-2])))))
  , test "3" (\_ -> equalHack "{\"Sum07A\":[2,-1,-3]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A [2,-1,-3]))))
  , test "4" (\_ -> equalHack "{\"Sum07A\":[]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A []))))
  , test "5" (\_ -> equalHack "{\"Sum07B\":[0,8,-6]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [0,8,-6])))))
  , test "6" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[5,7,3,2,-5,2,6,3]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [5,7,3,2,-5,2,6,3]}))))
  , test "7" (\_ -> equalHack "{\"Sum07E\":{\"bar\":-2,\"baz\":1}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07E {bar = -2, baz = 1}))))
  , test "8" (\_ -> equalHack "{\"Sum07E\":{\"bar\":8,\"baz\":-13}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07E {bar = 8, baz = -13}))))
  , test "9" (\_ -> equalHack "{\"Sum07B\":[-6,-4,16]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [-6,-4,16])))))
  , test "10" (\_ -> equalHack "{\"Sum07C\":[[13],[-11,-9,0,1,15,9,18,-2,-7,7,16,14,-1,17,9,-9,12,-16]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [13] [-11,-9,0,1,15,9,18,-2,-7,7,16,14,-1,17,9,-9,12,-16]))))
  , test "11" (\_ -> equalHack "{\"Sum07A\":[13,17,-14,-17,11,-16,1,-9,15,-5,10,-16,11]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A [13,17,-14,-17,11,-16,1,-9,15,-5,10,-16,11]))))
  ]

sumEncode08 : Test
sumEncode08 = describe "Sum encode 08"
  [ test "1" (\_ -> equalHack "{\"Sum08E\":{\"bar\":0,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"Sum08E\":{\"bar\":-2,\"baz\":2}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = -2, baz = 2}))))
  , test "3" (\_ -> equalHack "{\"Sum08E\":{\"bar\":-1,\"baz\":1}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = -1, baz = 1}))))
  , test "4" (\_ -> equalHack "{\"Sum08C\":[[3],[4,1,1]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [3] [4,1,1]))))
  , test "5" (\_ -> equalHack "{\"Sum08C\":[[-2,-7,-1,-1,-2,-3,-8,-3],[0,0]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [-2,-7,-1,-1,-2,-3,-8,-3] [0,0]))))
  , test "6" (\_ -> equalHack "{\"Sum08E\":{\"bar\":9,\"baz\":-7}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = 9, baz = -7}))))
  , test "7" (\_ -> equalHack "{\"Sum08B\":[-1,-1,7,2,2,12]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08B (Just [-1,-1,7,2,2,12])))))
  , test "8" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[-8,7,6]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [-8,7,6]}))))
  , test "9" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[8,-13,10,9]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [8,-13,10,9]}))))
  , test "10" (\_ -> equalHack "{\"Sum08B\":[-5,-2,1,17,14,1,-12,12,4,-1,5,2]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08B (Just [-5,-2,1,17,14,1,-12,12,4,-1,5,2])))))
  , test "11" (\_ -> equalHack "{\"Sum08C\":[[11,5,5,-1,10,-17,7,-17,1,-18,1],[-10,3,-6,1,-12,-19,-9,-1,0,-12,-2,-4,-16,-18,6,17,-3,16,4,-10]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [11,5,5,-1,10,-17,7,-17,1,-18,1] [-10,3,-6,1,-12,-19,-9,-1,0,-12,-2,-4,-16,-18,6,17,-3,16,4,-10]))))
  ]

sumEncode09 : Test
sumEncode09 = describe "Sum encode 09"
  [ test "1" (\_ -> equalHack "[\"Sum09E\",{\"bar\":0,\"baz\":0}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "[\"Sum09B\",[-1]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [-1])))))
  , test "3" (\_ -> equalHack "[\"Sum09C\",[[4,1,0],[-4,-4]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [4,1,0] [-4,-4]))))
  , test "4" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[1,5]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = [1,5]}))))
  , test "5" (\_ -> equalHack "[\"Sum09E\",{\"bar\":-4,\"baz\":5}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = -4, baz = 5}))))
  , test "6" (\_ -> equalHack "[\"Sum09A\",[-8,2,1,-3,-9,-9,7,8,-4]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09A [-8,2,1,-3,-9,-9,7,8,-4]))))
  , test "7" (\_ -> equalHack "[\"Sum09E\",{\"bar\":3,\"baz\":5}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = 3, baz = 5}))))
  , test "8" (\_ -> equalHack "[\"Sum09C\",[[-13,-7,2,-7,7,12,2,-6,10,10,0,2,3],[6,3,-4,-11,-2,1,13,-4,4,7,10,-6]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [-13,-7,2,-7,7,12,2,-6,10,10,0,2,3] [6,3,-4,-11,-2,1,13,-4,4,7,10,-6]))))
  , test "9" (\_ -> equalHack "[\"Sum09A\",[-7,-7,2,2,0,-13,-6,-8,13,-4,4,2,16,15,11]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09A [-7,-7,2,2,0,-13,-6,-8,13,-4,4,2,16,15,11]))))
  , test "10" (\_ -> equalHack "[\"Sum09C\",[[-3,9,-6,-8,-6,-17,12,-5,-9,-15,-17,10,14],[9,-8,-11,10,1,-10,14,13,17,12,-11,13,-1,-12,-14,-4,9,-11]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [-3,9,-6,-8,-6,-17,12,-5,-9,-15,-17,10,14] [9,-8,-11,10,1,-10,14,13,17,12,-11,13,-1,-12,-14,-4,9,-11]))))
  , test "11" (\_ -> equalHack "[\"Sum09B\",[-11,13,-14,-7,-6,11,-3,11,17]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [-11,13,-14,-7,-6,11,-3,11,17])))))
  ]

sumEncode10 : Test
sumEncode10 = describe "Sum encode 10"
  [ test "1" (\_ -> equalHack "[\"Sum10D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10D {foo = []}))))
  , test "2" (\_ -> equalHack "[\"Sum10A\",[2]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [2]))))
  , test "3" (\_ -> equalHack "[\"Sum10B\",[-3,3,1]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [-3,3,1])))))
  , test "4" (\_ -> equalHack "[\"Sum10E\",{\"bar\":3,\"baz\":5}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 3, baz = 5}))))
  , test "5" (\_ -> equalHack "[\"Sum10B\",[3]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [3])))))
  , test "6" (\_ -> equalHack "[\"Sum10B\",[-6,-1,2,2,-9,-9,8,0,8,4]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [-6,-1,2,2,-9,-9,8,0,8,4])))))
  , test "7" (\_ -> equalHack "[\"Sum10A\",[-12,2,6,7,1,5,-1,-12,-9]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [-12,2,6,7,1,5,-1,-12,-9]))))
  , test "8" (\_ -> equalHack "[\"Sum10A\",[-1,4,-2,-1,-2,1,2,-11,6,-2,-5,6,-2,-7]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [-1,4,-2,-1,-2,1,2,-11,6,-2,-5,6,-2,-7]))))
  , test "9" (\_ -> equalHack "[\"Sum10E\",{\"bar\":15,\"baz\":15}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 15, baz = 15}))))
  , test "10" (\_ -> equalHack "[\"Sum10B\",[-12,-1,13,15,8,-7,-13,6,2,7,-11,-6,10,-13,18,-9,2,5]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [-12,-1,13,15,8,-7,-13,6,2,7,-11,-6,10,-13,18,-9,2,5])))))
  , test "11" (\_ -> equalHack "[\"Sum10B\",[15,17,-1,-3,-19,-12,-20,-10,1,-17,14,-8,13]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [15,17,-1,-3,-19,-12,-20,-10,1,-17,14,-8,13])))))
  ]

sumEncode11 : Test
sumEncode11 = describe "Sum encode 11"
  [ test "1" (\_ -> equalHack "[\"Sum11E\",{\"bar\":0,\"baz\":0}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "[\"Sum11A\",[-2]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [-2]))))
  , test "3" (\_ -> equalHack "[\"Sum11C\",[[2,-2,2,-1],[2,1,-1,0]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [2,-2,2,-1] [2,1,-1,0]))))
  , test "4" (\_ -> equalHack "[\"Sum11E\",{\"bar\":0,\"baz\":-4}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11E {bar = 0, baz = -4}))))
  , test "5" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = []}))))
  , test "6" (\_ -> equalHack "[\"Sum11C\",[[1,-3,-8,-2,-3,9,-3,-8,3],[]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [1,-3,-8,-2,-3,9,-3,-8,3] []))))
  , test "7" (\_ -> equalHack "[\"Sum11A\",[0,0,0,-1,-6,-3,0,3,-7]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [0,0,0,-1,-6,-3,0,3,-7]))))
  , test "8" (\_ -> equalHack "[\"Sum11A\",[11,0,10,-13,-9,-5,-4,3]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [11,0,10,-13,-9,-5,-4,3]))))
  , test "9" (\_ -> equalHack "[\"Sum11B\",[0,12,-11,-14,11,10,4]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11B (Just [0,12,-11,-14,11,10,4])))))
  , test "10" (\_ -> equalHack "[\"Sum11B\",[13,0,-17,6,-9,3,-8,1,11,-9]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11B (Just [13,0,-17,6,-9,3,-8,1,11,-9])))))
  , test "11" (\_ -> equalHack "[\"Sum11B\",[-1,-11,-13,-14,13]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11B (Just [-1,-11,-13,-14,13])))))
  ]

sumEncode12 : Test
sumEncode12 = describe "Sum encode 12"
  [ test "1" (\_ -> equalHack "[\"Sum12A\",[]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A []))))
  , test "2" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[-2,-2]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = [-2,-2]}))))
  , test "3" (\_ -> equalHack "[\"Sum12B\",[3,-4]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [3,-4])))))
  , test "4" (\_ -> equalHack "[\"Sum12C\",[[],[]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12C [] []))))
  , test "5" (\_ -> equalHack "[\"Sum12C\",[[-1,4,2,3,-7,-4],[]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12C [-1,4,2,3,-7,-4] []))))
  , test "6" (\_ -> equalHack "[\"Sum12B\",[4,-3,-1,-1,-2,0,2,-10,9]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [4,-3,-1,-1,-2,0,2,-10,9])))))
  , test "7" (\_ -> equalHack "[\"Sum12A\",[1,-12,-7,-1,-6,-12,-9,-2,-2,10,-3]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A [1,-12,-7,-1,-6,-12,-9,-2,-2,10,-3]))))
  , test "8" (\_ -> equalHack "[\"Sum12B\",[11,10,-10,5,5,-11,-4,11,-6,9,-12,-13,-3]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [11,10,-10,5,5,-11,-4,11,-6,9,-12,-13,-3])))))
  , test "9" (\_ -> equalHack "[\"Sum12A\",[-13,-16,11,-5,-14,15,15,6]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A [-13,-16,11,-5,-14,15,15,6]))))
  , test "10" (\_ -> equalHack "[\"Sum12B\",[18,-4,-8]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [18,-4,-8])))))
  , test "11" (\_ -> equalHack "[\"Sum12C\",[[2,10,-19,17,-3,13,-11,-6,5,-13,-3,-18,-20],[-19,-14,-18,-17,15,-7,-9,-18,2,6,19,2,12,16,-19,-18,4,-2]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12C [2,10,-19,17,-3,13,-11,-6,5,-13,-3,-18,-20] [-19,-14,-18,-17,15,-7,-9,-18,2,6,19,2,12,16,-19,-18,4,-2]))))
  ]

sumDecode01 : Test
sumDecode01 = describe "Sum decode 01"
  [ test "1" (\_ -> equal (Ok (Sum01C [] [])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum01D {foo = []})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[]}"))
  , test "3" (\_ -> equal (Ok (Sum01E {bar = -1, baz = -4})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":-1,\"baz\":-4}"))
  , test "4" (\_ -> equal (Ok (Sum01B (Just [1,-4,1]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[1,-4,1]}"))
  , test "5" (\_ -> equal (Ok (Sum01E {bar = -6, baz = -2})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":-6,\"baz\":-2}"))
  , test "6" (\_ -> equal (Ok (Sum01A [5,1,-5,10,-2,9])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[5,1,-5,10,-2,9]}"))
  , test "7" (\_ -> equal (Ok (Sum01A [-8,12,-11,-7,-12,-11,9,2,6,-7,2,11])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-8,12,-11,-7,-12,-11,9,2,6,-7,2,11]}"))
  , test "8" (\_ -> equal (Ok (Sum01A [-11,-4,10,14,1,-8,-4,7,2,-2,2,-6,-14,3])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-11,-4,10,14,1,-8,-4,7,2,-2,2,-6,-14,3]}"))
  , test "9" (\_ -> equal (Ok (Sum01A [])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[]}"))
  , test "10" (\_ -> equal (Ok (Sum01B (Just [-13,13,-4,-13,5,-17,3,-18,12,9,1,13]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[-13,13,-4,-13,5,-17,3,-18,12,9,1,13]}"))
  , test "11" (\_ -> equal (Ok (Sum01C [18,17,12,19,18,8,-15,-19,-18,-18,-11,-18,19,9,7] [-19,-19,-19,5,-6,17,-9,-12,-17,-9,11,-1,9,-13,0])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[18,17,12,19,18,8,-15,-19,-18,-18,-11,-18,19,9,7],[-19,-19,-19,5,-6,17,-9,-12,-17,-9,11,-1,9,-13,0]]}"))
  ]

sumDecode02 : Test
sumDecode02 = describe "Sum decode 02"
  [ test "1" (\_ -> equal (Ok (Sum02D {foo = []})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum02E {bar = -1, baz = 2})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":-1,\"baz\":2}"))
  , test "3" (\_ -> equal (Ok (Sum02B (Just []))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[]}"))
  , test "4" (\_ -> equal (Ok (Sum02C [3,6,-6,-4,0] [-3,4,-4])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[3,6,-6,-4,0],[-3,4,-4]]}"))
  , test "5" (\_ -> equal (Ok (Sum02E {bar = -6, baz = -8})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":-6,\"baz\":-8}"))
  , test "6" (\_ -> equal (Ok (Sum02A [7,-4])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[7,-4]}"))
  , test "7" (\_ -> equal (Ok (Sum02B (Just [-7,-10,8,2,-2,3,-11]))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[-7,-10,8,2,-2,3,-11]}"))
  , test "8" (\_ -> equal (Ok (Sum02A [-2,12,12,6,14,5,-14,1])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[-2,12,12,6,14,5,-14,1]}"))
  , test "9" (\_ -> equal (Ok (Sum02E {bar = 4, baz = 2})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":4,\"baz\":2}"))
  , test "10" (\_ -> equal (Ok (Sum02E {bar = 14, baz = 17})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":14,\"baz\":17}"))
  , test "11" (\_ -> equal (Ok (Sum02C [-19,-1,-9,-20,-7,-4,-14,1,17,14,7,12,18,-10] [9,-15,13,-5,13,-12,8,16])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[-19,-1,-9,-20,-7,-4,-14,1,17,14,7,12,18,-10],[9,-15,13,-5,13,-12,8,16]]}"))
  ]

sumDecode03 : Test
sumDecode03 = describe "Sum decode 03"
  [ test "1" (\_ -> equal (Ok (Sum03E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":0,\"baz\":0}"))
  , test "2" (\_ -> equal (Ok (Sum03E {bar = -2, baz = -2})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":-2,\"baz\":-2}"))
  , test "3" (\_ -> equal (Ok (Sum03A [-4,4,-3])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[-4,4,-3]}"))
  , test "4" (\_ -> equal (Ok (Sum03E {bar = -3, baz = 2})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":-3,\"baz\":2}"))
  , test "5" (\_ -> equal (Ok (Sum03B (Just []))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[]}"))
  , test "6" (\_ -> equal (Ok (Sum03E {bar = 3, baz = -8})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":3,\"baz\":-8}"))
  , test "7" (\_ -> equal (Ok (Sum03E {bar = -8, baz = 0})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":-8,\"baz\":0}"))
  , test "8" (\_ -> equal (Ok (Sum03B (Just [-7,-2,2,-4,-5,-1,4]))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[-7,-2,2,-4,-5,-1,4]}"))
  , test "9" (\_ -> equal (Ok (Sum03E {bar = 8, baz = -15})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":8,\"baz\":-15}"))
  , test "10" (\_ -> equal (Ok (Sum03A [10,-8,-16,6,15,-1,13,-15,-6,2,-3,8])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[10,-8,-16,6,15,-1,13,-15,-6,2,-3,8]}"))
  , test "11" (\_ -> equal (Ok (Sum03C [13,1,-6,-3,18,3,18,-18,10,-18,-16,14,15,19,-13,-10] [-2,-17,-17,10,-2,5,-17])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[13,1,-6,-3,18,3,18,-18,10,-18,-16,14,15,19,-13,-10],[-2,-17,-17,10,-2,5,-17]]}"))
  ]

sumDecode04 : Test
sumDecode04 = describe "Sum decode 04"
  [ test "1" (\_ -> equal (Ok (Sum04D {foo = []})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum04C [2,1] [-2])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[2,1],[-2]]}"))
  , test "3" (\_ -> equal (Ok (Sum04B (Just [-3]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[-3]}"))
  , test "4" (\_ -> equal (Ok (Sum04D {foo = [2,5,5,4]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[2,5,5,4]}"))
  , test "5" (\_ -> equal (Ok (Sum04C [0,-3,-3,2,-2,-1] [8,0])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[0,-3,-3,2,-2,-1],[8,0]]}"))
  , test "6" (\_ -> equal (Ok (Sum04D {foo = [-5,6,7,-2,3,5,10,-6]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[-5,6,7,-2,3,5,10,-6]}"))
  , test "7" (\_ -> equal (Ok (Sum04B (Just [-6]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[-6]}"))
  , test "8" (\_ -> equal (Ok (Sum04D {foo = [11,3,11,-6,-3,-4,10,5]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[11,3,11,-6,-3,-4,10,5]}"))
  , test "9" (\_ -> equal (Ok (Sum04D {foo = [9,16,-15,7,14,15,12,8,-2,-10,10,-12]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[9,16,-15,7,14,15,12,8,-2,-10,10,-12]}"))
  , test "10" (\_ -> equal (Ok (Sum04E {bar = -11, baz = 6})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04E\",\"bar\":-11,\"baz\":6}"))
  , test "11" (\_ -> equal (Ok (Sum04D {foo = []})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[]}"))
  ]

sumDecode05 : Test
sumDecode05 = describe "Sum decode 05"
  [ test "1" (\_ -> equal (Ok (Sum05D {foo = []})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[]}}"))
  , test "2" (\_ -> equal (Ok (Sum05A [])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[]}"))
  , test "3" (\_ -> equal (Ok (Sum05C [4,-3,-3,2] [-2])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[4,-3,-3,2],[-2]]}"))
  , test "4" (\_ -> equal (Ok (Sum05B (Just [-6,0,2,-1,0]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[-6,0,2,-1,0]}"))
  , test "5" (\_ -> equal (Ok (Sum05E {bar = 4, baz = 8})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":4,\"baz\":8}}"))
  , test "6" (\_ -> equal (Ok (Sum05D {foo = [1,-7,8,8,-5,-9,10,-7]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[1,-7,8,8,-5,-9,10,-7]}}"))
  , test "7" (\_ -> equal (Ok (Sum05C [8,8,-11,-2,-10,-10,9,-8,-10,9] [9,-10,12,10])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[8,8,-11,-2,-10,-10,9,-8,-10,9],[9,-10,12,10]]}"))
  , test "8" (\_ -> equal (Ok (Sum05E {bar = -14, baz = 1})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-14,\"baz\":1}}"))
  , test "9" (\_ -> equal (Ok (Sum05D {foo = [-6,12,1,6,-3,2,-3,2,-16,9,-14,-5]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[-6,12,1,6,-3,2,-3,2,-16,9,-14,-5]}}"))
  , test "10" (\_ -> equal (Ok (Sum05B (Just [-16,-1,-4,3,-17,-11,12,8,10,12,-17]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[-16,-1,-4,3,-17,-11,12,8,10,12,-17]}"))
  , test "11" (\_ -> equal (Ok (Sum05E {bar = -3, baz = 8})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-3,\"baz\":8}}"))
  ]

sumDecode06 : Test
sumDecode06 = describe "Sum decode 06"
  [ test "1" (\_ -> equal (Ok (Sum06C [] [])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum06C [-1] [])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[-1],[]]}"))
  , test "3" (\_ -> equal (Ok (Sum06C [-1,-1,0,-1] [2,4])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[-1,-1,0,-1],[2,4]]}"))
  , test "4" (\_ -> equal (Ok (Sum06B (Just [4,0,-3,2]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[4,0,-3,2]}"))
  , test "5" (\_ -> equal (Ok (Sum06D {foo = [-7,8,1,0,4,4,-2]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[-7,8,1,0,4,4,-2]}}"))
  , test "6" (\_ -> equal (Ok (Sum06A [-7,0,10,-9,6,-7,-8,3])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[-7,0,10,-9,6,-7,-8,3]}"))
  , test "7" (\_ -> equal (Ok (Sum06C [-10,11] [6,2,7,8,3])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[-10,11],[6,2,7,8,3]]}"))
  , test "8" (\_ -> equal (Ok (Sum06E {bar = -3, baz = -6})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":-3,\"baz\":-6}}"))
  , test "9" (\_ -> equal (Ok (Sum06E {bar = -14, baz = 7})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":-14,\"baz\":7}}"))
  , test "10" (\_ -> equal (Ok (Sum06C [-12] [10,18,-13])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[-12],[10,18,-13]]}"))
  , test "11" (\_ -> equal (Ok (Sum06A [15,-13,9,-3])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[15,-13,9,-3]}"))
  ]

sumDecode07 : Test
sumDecode07 = describe "Sum decode 07"
  [ test "1" (\_ -> equal (Ok (Sum07A [])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum07B (Just [-2]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[-2]}"))
  , test "3" (\_ -> equal (Ok (Sum07A [2,-1,-3])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[2,-1,-3]}"))
  , test "4" (\_ -> equal (Ok (Sum07A [])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[]}"))
  , test "5" (\_ -> equal (Ok (Sum07B (Just [0,8,-6]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[0,8,-6]}"))
  , test "6" (\_ -> equal (Ok (Sum07D {foo = [5,7,3,2,-5,2,6,3]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[5,7,3,2,-5,2,6,3]}}"))
  , test "7" (\_ -> equal (Ok (Sum07E {bar = -2, baz = 1})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":-2,\"baz\":1}}"))
  , test "8" (\_ -> equal (Ok (Sum07E {bar = 8, baz = -13})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":8,\"baz\":-13}}"))
  , test "9" (\_ -> equal (Ok (Sum07B (Just [-6,-4,16]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[-6,-4,16]}"))
  , test "10" (\_ -> equal (Ok (Sum07C [13] [-11,-9,0,1,15,9,18,-2,-7,7,16,14,-1,17,9,-9,12,-16])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[13],[-11,-9,0,1,15,9,18,-2,-7,7,16,14,-1,17,9,-9,12,-16]]}"))
  , test "11" (\_ -> equal (Ok (Sum07A [13,17,-14,-17,11,-16,1,-9,15,-5,10,-16,11])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[13,17,-14,-17,11,-16,1,-9,15,-5,10,-16,11]}"))
  ]

sumDecode08 : Test
sumDecode08 = describe "Sum decode 08"
  [ test "1" (\_ -> equal (Ok (Sum08E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":0,\"baz\":0}}"))
  , test "2" (\_ -> equal (Ok (Sum08E {bar = -2, baz = 2})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":-2,\"baz\":2}}"))
  , test "3" (\_ -> equal (Ok (Sum08E {bar = -1, baz = 1})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":-1,\"baz\":1}}"))
  , test "4" (\_ -> equal (Ok (Sum08C [3] [4,1,1])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[3],[4,1,1]]}"))
  , test "5" (\_ -> equal (Ok (Sum08C [-2,-7,-1,-1,-2,-3,-8,-3] [0,0])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[-2,-7,-1,-1,-2,-3,-8,-3],[0,0]]}"))
  , test "6" (\_ -> equal (Ok (Sum08E {bar = 9, baz = -7})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":9,\"baz\":-7}}"))
  , test "7" (\_ -> equal (Ok (Sum08B (Just [-1,-1,7,2,2,12]))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[-1,-1,7,2,2,12]}"))
  , test "8" (\_ -> equal (Ok (Sum08D {foo = [-8,7,6]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[-8,7,6]}}"))
  , test "9" (\_ -> equal (Ok (Sum08D {foo = [8,-13,10,9]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[8,-13,10,9]}}"))
  , test "10" (\_ -> equal (Ok (Sum08B (Just [-5,-2,1,17,14,1,-12,12,4,-1,5,2]))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[-5,-2,1,17,14,1,-12,12,4,-1,5,2]}"))
  , test "11" (\_ -> equal (Ok (Sum08C [11,5,5,-1,10,-17,7,-17,1,-18,1] [-10,3,-6,1,-12,-19,-9,-1,0,-12,-2,-4,-16,-18,6,17,-3,16,4,-10])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[11,5,5,-1,10,-17,7,-17,1,-18,1],[-10,3,-6,1,-12,-19,-9,-1,0,-12,-2,-4,-16,-18,6,17,-3,16,4,-10]]}"))
  ]

sumDecode09 : Test
sumDecode09 = describe "Sum decode 09"
  [ test "1" (\_ -> equal (Ok (Sum09E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":0,\"baz\":0}]"))
  , test "2" (\_ -> equal (Ok (Sum09B (Just [-1]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[-1]]"))
  , test "3" (\_ -> equal (Ok (Sum09C [4,1,0] [-4,-4])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[4,1,0],[-4,-4]]]"))
  , test "4" (\_ -> equal (Ok (Sum09D {foo = [1,5]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[1,5]}]"))
  , test "5" (\_ -> equal (Ok (Sum09E {bar = -4, baz = 5})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":-4,\"baz\":5}]"))
  , test "6" (\_ -> equal (Ok (Sum09A [-8,2,1,-3,-9,-9,7,8,-4])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[-8,2,1,-3,-9,-9,7,8,-4]]"))
  , test "7" (\_ -> equal (Ok (Sum09E {bar = 3, baz = 5})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":3,\"baz\":5}]"))
  , test "8" (\_ -> equal (Ok (Sum09C [-13,-7,2,-7,7,12,2,-6,10,10,0,2,3] [6,3,-4,-11,-2,1,13,-4,4,7,10,-6])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[-13,-7,2,-7,7,12,2,-6,10,10,0,2,3],[6,3,-4,-11,-2,1,13,-4,4,7,10,-6]]]"))
  , test "9" (\_ -> equal (Ok (Sum09A [-7,-7,2,2,0,-13,-6,-8,13,-4,4,2,16,15,11])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[-7,-7,2,2,0,-13,-6,-8,13,-4,4,2,16,15,11]]"))
  , test "10" (\_ -> equal (Ok (Sum09C [-3,9,-6,-8,-6,-17,12,-5,-9,-15,-17,10,14] [9,-8,-11,10,1,-10,14,13,17,12,-11,13,-1,-12,-14,-4,9,-11])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[-3,9,-6,-8,-6,-17,12,-5,-9,-15,-17,10,14],[9,-8,-11,10,1,-10,14,13,17,12,-11,13,-1,-12,-14,-4,9,-11]]]"))
  , test "11" (\_ -> equal (Ok (Sum09B (Just [-11,13,-14,-7,-6,11,-3,11,17]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[-11,13,-14,-7,-6,11,-3,11,17]]"))
  ]

sumDecode10 : Test
sumDecode10 = describe "Sum decode 10"
  [ test "1" (\_ -> equal (Ok (Sum10D {foo = []})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10D\",{\"foo\":[]}]"))
  , test "2" (\_ -> equal (Ok (Sum10A [2])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[2]]"))
  , test "3" (\_ -> equal (Ok (Sum10B (Just [-3,3,1]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[-3,3,1]]"))
  , test "4" (\_ -> equal (Ok (Sum10E {bar = 3, baz = 5})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":3,\"baz\":5}]"))
  , test "5" (\_ -> equal (Ok (Sum10B (Just [3]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[3]]"))
  , test "6" (\_ -> equal (Ok (Sum10B (Just [-6,-1,2,2,-9,-9,8,0,8,4]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[-6,-1,2,2,-9,-9,8,0,8,4]]"))
  , test "7" (\_ -> equal (Ok (Sum10A [-12,2,6,7,1,5,-1,-12,-9])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[-12,2,6,7,1,5,-1,-12,-9]]"))
  , test "8" (\_ -> equal (Ok (Sum10A [-1,4,-2,-1,-2,1,2,-11,6,-2,-5,6,-2,-7])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[-1,4,-2,-1,-2,1,2,-11,6,-2,-5,6,-2,-7]]"))
  , test "9" (\_ -> equal (Ok (Sum10E {bar = 15, baz = 15})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":15,\"baz\":15}]"))
  , test "10" (\_ -> equal (Ok (Sum10B (Just [-12,-1,13,15,8,-7,-13,6,2,7,-11,-6,10,-13,18,-9,2,5]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[-12,-1,13,15,8,-7,-13,6,2,7,-11,-6,10,-13,18,-9,2,5]]"))
  , test "11" (\_ -> equal (Ok (Sum10B (Just [15,17,-1,-3,-19,-12,-20,-10,1,-17,14,-8,13]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[15,17,-1,-3,-19,-12,-20,-10,1,-17,14,-8,13]]"))
  ]

sumDecode11 : Test
sumDecode11 = describe "Sum decode 11"
  [ test "1" (\_ -> equal (Ok (Sum11E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":0,\"baz\":0}]"))
  , test "2" (\_ -> equal (Ok (Sum11A [-2])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[-2]]"))
  , test "3" (\_ -> equal (Ok (Sum11C [2,-2,2,-1] [2,1,-1,0])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[2,-2,2,-1],[2,1,-1,0]]]"))
  , test "4" (\_ -> equal (Ok (Sum11E {bar = 0, baz = -4})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":0,\"baz\":-4}]"))
  , test "5" (\_ -> equal (Ok (Sum11D {foo = []})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[]}]"))
  , test "6" (\_ -> equal (Ok (Sum11C [1,-3,-8,-2,-3,9,-3,-8,3] [])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[1,-3,-8,-2,-3,9,-3,-8,3],[]]]"))
  , test "7" (\_ -> equal (Ok (Sum11A [0,0,0,-1,-6,-3,0,3,-7])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[0,0,0,-1,-6,-3,0,3,-7]]"))
  , test "8" (\_ -> equal (Ok (Sum11A [11,0,10,-13,-9,-5,-4,3])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[11,0,10,-13,-9,-5,-4,3]]"))
  , test "9" (\_ -> equal (Ok (Sum11B (Just [0,12,-11,-14,11,10,4]))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[0,12,-11,-14,11,10,4]]"))
  , test "10" (\_ -> equal (Ok (Sum11B (Just [13,0,-17,6,-9,3,-8,1,11,-9]))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[13,0,-17,6,-9,3,-8,1,11,-9]]"))
  , test "11" (\_ -> equal (Ok (Sum11B (Just [-1,-11,-13,-14,13]))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[-1,-11,-13,-14,13]]"))
  ]

sumDecode12 : Test
sumDecode12 = describe "Sum decode 12"
  [ test "1" (\_ -> equal (Ok (Sum12A [])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum12D {foo = [-2,-2]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[-2,-2]}]"))
  , test "3" (\_ -> equal (Ok (Sum12B (Just [3,-4]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[3,-4]]"))
  , test "4" (\_ -> equal (Ok (Sum12C [] [])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[],[]]]"))
  , test "5" (\_ -> equal (Ok (Sum12C [-1,4,2,3,-7,-4] [])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[-1,4,2,3,-7,-4],[]]]"))
  , test "6" (\_ -> equal (Ok (Sum12B (Just [4,-3,-1,-1,-2,0,2,-10,9]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[4,-3,-1,-1,-2,0,2,-10,9]]"))
  , test "7" (\_ -> equal (Ok (Sum12A [1,-12,-7,-1,-6,-12,-9,-2,-2,10,-3])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[1,-12,-7,-1,-6,-12,-9,-2,-2,10,-3]]"))
  , test "8" (\_ -> equal (Ok (Sum12B (Just [11,10,-10,5,5,-11,-4,11,-6,9,-12,-13,-3]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[11,10,-10,5,5,-11,-4,11,-6,9,-12,-13,-3]]"))
  , test "9" (\_ -> equal (Ok (Sum12A [-13,-16,11,-5,-14,15,15,6])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[-13,-16,11,-5,-14,15,15,6]]"))
  , test "10" (\_ -> equal (Ok (Sum12B (Just [18,-4,-8]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[18,-4,-8]]"))
  , test "11" (\_ -> equal (Ok (Sum12C [2,10,-19,17,-3,13,-11,-6,5,-13,-3,-18,-20] [-19,-14,-18,-17,15,-7,-9,-18,2,6,19,2,12,16,-19,-18,4,-2])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[2,10,-19,17,-3,13,-11,-6,5,-13,-3,-18,-20],[-19,-14,-18,-17,15,-7,-9,-18,2,6,19,2,12,16,-19,-18,4,-2]]]"))
  ]

recordDecode1 : Test
recordDecode1 = describe "Record decode 1"
  [ test "1" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"))
  , test "2" (\_ -> equal (Ok (Record1 {foo = -2, bar = Just 2, baz = [], qux = Just [1,-1], jmap = fromList [("a",-1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-2,\"bar\":2,\"baz\":[],\"qux\":[1,-1],\"jmap\":{\"a\":-1}}"))
  , test "3" (\_ -> equal (Ok (Record1 {foo = -2, bar = Just 1, baz = [], qux = Just [2], jmap = fromList [("a",2)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-2,\"bar\":1,\"baz\":[],\"qux\":[2],\"jmap\":{\"a\":2}}"))
  , test "4" (\_ -> equal (Ok (Record1 {foo = -3, bar = Just 2, baz = [], qux = Just [-6,4,5,6], jmap = fromList [("a",-5)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":2,\"baz\":[],\"qux\":[-6,4,5,6],\"jmap\":{\"a\":-5}}"))
  , test "5" (\_ -> equal (Ok (Record1 {foo = -2, bar = Just (-8), baz = [], qux = Just [5], jmap = fromList [("a",7)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-2,\"bar\":-8,\"baz\":[],\"qux\":[5],\"jmap\":{\"a\":7}}"))
  , test "6" (\_ -> equal (Ok (Record1 {foo = 10, bar = Just 3, baz = [10,4,7,3], qux = Just [4,-1,10,9,-2,10,-1,-1,-4,4], jmap = fromList [("a",5)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":10,\"bar\":3,\"baz\":[10,4,7,3],\"qux\":[4,-1,10,9,-2,10,-1,-1,-4,4],\"jmap\":{\"a\":5}}"))
  , test "7" (\_ -> equal (Ok (Record1 {foo = 8, bar = Just 4, baz = [12,-7,-3,-8,11,10], qux = Just [-10,9,-8,0,6,2], jmap = fromList [("a",7)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":8,\"bar\":4,\"baz\":[12,-7,-3,-8,11,10],\"qux\":[-10,9,-8,0,6,2],\"jmap\":{\"a\":7}}"))
  , test "8" (\_ -> equal (Ok (Record1 {foo = -12, bar = Just (-12), baz = [4,10,3,-13,-8,-14,6,9,-3,6,-7,-1,9,-10], qux = Just [10,5,3,14], jmap = fromList [("a",-6)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-12,\"bar\":-12,\"baz\":[4,10,3,-13,-8,-14,6,9,-3,6,-7,-1,9,-10],\"qux\":[10,5,3,14],\"jmap\":{\"a\":-6}}"))
  , test "9" (\_ -> equal (Ok (Record1 {foo = -4, bar = Just (-14), baz = [-4,-10,-6,-10,0,-1,-6,13,0,-16,9], qux = Just [10,11,10,8,16,-13], jmap = fromList [("a",6)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-4,\"bar\":-14,\"baz\":[-4,-10,-6,-10,0,-1,-6,13,0,-16,9],\"qux\":[10,11,10,8,16,-13],\"jmap\":{\"a\":6}}"))
  , test "10" (\_ -> equal (Ok (Record1 {foo = -11, bar = Just 18, baz = [-6,18,15,9,-15,-18,-11], qux = Just [11,-11,-16,-13,3,-5,14,4], jmap = fromList [("a",-7)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-11,\"bar\":18,\"baz\":[-6,18,15,9,-15,-18,-11],\"qux\":[11,-11,-16,-13,3,-5,14,4],\"jmap\":{\"a\":-7}}"))
  , test "11" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 9, baz = [5,-6,-9,7,9,19,12], qux = Just [5,-12,-14,-5,-1,19,2,10,0], jmap = fromList [("a",16)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":9,\"baz\":[5,-6,-9,7,9,19,12],\"qux\":[5,-12,-14,-5,-1,19,2,10,0],\"jmap\":{\"a\":16}}"))
  ]

recordDecode2 : Test
recordDecode2 = describe "Record decode 2"
  [ test "1" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 2, baz = [2,-2], qux = Just [2]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":2,\"baz\":[2,-2],\"qux\":[2]}"))
  , test "3" (\_ -> equal (Ok (Record2 {foo = -4, bar = Just (-1), baz = [2,-3,1], qux = Just [-2,4,4,-4]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-4,\"bar\":-1,\"baz\":[2,-3,1],\"qux\":[-2,4,4,-4]}"))
  , test "4" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 4, baz = [1,-6,-3,-6,3,0], qux = Just [0,6,2,1,2]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":4,\"baz\":[1,-6,-3,-6,3,0],\"qux\":[0,6,2,1,2]}"))
  , test "5" (\_ -> equal (Ok (Record2 {foo = 6, bar = Just (-2), baz = [-6,-6,-7,-2], qux = Just [7,-1]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":6,\"bar\":-2,\"baz\":[-6,-6,-7,-2],\"qux\":[7,-1]}"))
  , test "6" (\_ -> equal (Ok (Record2 {foo = -3, bar = Just 8, baz = [2,6,1,-4,6], qux = Just [10,2,-7,2,10,-7,9,7,-10]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":8,\"baz\":[2,6,1,-4,6],\"qux\":[10,2,-7,2,10,-7,9,7,-10]}"))
  , test "7" (\_ -> equal (Ok (Record2 {foo = 9, bar = Just 11, baz = [-5,5,0,-3,-1,4,-1,-1,8,-8], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":9,\"bar\":11,\"baz\":[-5,5,0,-3,-1,4,-1,-1,8,-8],\"qux\":[]}"))
  , test "8" (\_ -> equal (Ok (Record2 {foo = -7, bar = Just (-5), baz = [-10,-13,-2,-13], qux = Just [3,-8,12,-2,-14]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-7,\"bar\":-5,\"baz\":[-10,-13,-2,-13],\"qux\":[3,-8,12,-2,-14]}"))
  , test "9" (\_ -> equal (Ok (Record2 {foo = 11, bar = Just (-9), baz = [-6,5,5,8,-3,1,-2,5,-4,-7,2,-2,-10,0,-1,-5], qux = Just [-14,3,-15,-13,-6,-16]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":11,\"bar\":-9,\"baz\":[-6,5,5,8,-3,1,-2,5,-4,-7,2,-2,-10,0,-1,-5],\"qux\":[-14,3,-15,-13,-6,-16]}"))
  , test "10" (\_ -> equal (Ok (Record2 {foo = 8, bar = Just (-13), baz = [5,2,-16,1,8,-14,3,-10,-11,-16], qux = Just [-1,1,16,-3,9,-11,7,5,-14,1,-2,-10,9,7,-7]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":8,\"bar\":-13,\"baz\":[5,2,-16,1,8,-14,3,-10,-11,-16],\"qux\":[-1,1,16,-3,9,-11,7,5,-14,1,-2,-10,9,7,-7]}"))
  , test "11" (\_ -> equal (Ok (Record2 {foo = -19, bar = Just 18, baz = [8,14,4,2,5,11,12,-18,12], qux = Just [-10,-16,-4,1,4,13,3,13,8,16,15,-13,11,6,19]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-19,\"bar\":18,\"baz\":[8,14,4,2,5,11,12,-18,12],\"qux\":[-10,-16,-4,1,4,13,3,13,8,16,15,-13,11,6,19]}"))
  ]

recordEncode1 : Test
recordEncode1 = describe "Record encode 1"
  [ test "1" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]}))))
  , test "2" (\_ -> equalHack "{\"foo\":-2,\"bar\":2,\"baz\":[],\"qux\":[1,-1],\"jmap\":{\"a\":-1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -2, bar = Just 2, baz = [], qux = Just [1,-1], jmap = fromList [("a",-1)]}))))
  , test "3" (\_ -> equalHack "{\"foo\":-2,\"bar\":1,\"baz\":[],\"qux\":[2],\"jmap\":{\"a\":2}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -2, bar = Just 1, baz = [], qux = Just [2], jmap = fromList [("a",2)]}))))
  , test "4" (\_ -> equalHack "{\"foo\":-3,\"bar\":2,\"baz\":[],\"qux\":[-6,4,5,6],\"jmap\":{\"a\":-5}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -3, bar = Just 2, baz = [], qux = Just [-6,4,5,6], jmap = fromList [("a",-5)]}))))
  , test "5" (\_ -> equalHack "{\"foo\":-2,\"bar\":-8,\"baz\":[],\"qux\":[5],\"jmap\":{\"a\":7}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -2, bar = Just (-8), baz = [], qux = Just [5], jmap = fromList [("a",7)]}))))
  , test "6" (\_ -> equalHack "{\"foo\":10,\"bar\":3,\"baz\":[10,4,7,3],\"qux\":[4,-1,10,9,-2,10,-1,-1,-4,4],\"jmap\":{\"a\":5}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 10, bar = Just 3, baz = [10,4,7,3], qux = Just [4,-1,10,9,-2,10,-1,-1,-4,4], jmap = fromList [("a",5)]}))))
  , test "7" (\_ -> equalHack "{\"foo\":8,\"bar\":4,\"baz\":[12,-7,-3,-8,11,10],\"qux\":[-10,9,-8,0,6,2],\"jmap\":{\"a\":7}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 8, bar = Just 4, baz = [12,-7,-3,-8,11,10], qux = Just [-10,9,-8,0,6,2], jmap = fromList [("a",7)]}))))
  , test "8" (\_ -> equalHack "{\"foo\":-12,\"bar\":-12,\"baz\":[4,10,3,-13,-8,-14,6,9,-3,6,-7,-1,9,-10],\"qux\":[10,5,3,14],\"jmap\":{\"a\":-6}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -12, bar = Just (-12), baz = [4,10,3,-13,-8,-14,6,9,-3,6,-7,-1,9,-10], qux = Just [10,5,3,14], jmap = fromList [("a",-6)]}))))
  , test "9" (\_ -> equalHack "{\"foo\":-4,\"bar\":-14,\"baz\":[-4,-10,-6,-10,0,-1,-6,13,0,-16,9],\"qux\":[10,11,10,8,16,-13],\"jmap\":{\"a\":6}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -4, bar = Just (-14), baz = [-4,-10,-6,-10,0,-1,-6,13,0,-16,9], qux = Just [10,11,10,8,16,-13], jmap = fromList [("a",6)]}))))
  , test "10" (\_ -> equalHack "{\"foo\":-11,\"bar\":18,\"baz\":[-6,18,15,9,-15,-18,-11],\"qux\":[11,-11,-16,-13,3,-5,14,4],\"jmap\":{\"a\":-7}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -11, bar = Just 18, baz = [-6,18,15,9,-15,-18,-11], qux = Just [11,-11,-16,-13,3,-5,14,4], jmap = fromList [("a",-7)]}))))
  , test "11" (\_ -> equalHack "{\"foo\":0,\"bar\":9,\"baz\":[5,-6,-9,7,9,19,12],\"qux\":[5,-12,-14,-5,-1,19,2,10,0],\"jmap\":{\"a\":16}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just 9, baz = [5,-6,-9,7,9,19,12], qux = Just [5,-12,-14,-5,-1,19,2,10,0], jmap = fromList [("a",16)]}))))
  ]

recordEncode2 : Test
recordEncode2 = describe "Record encode 2"
  [ test "1" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []}))))
  , test "2" (\_ -> equalHack "{\"foo\":0,\"bar\":2,\"baz\":[2,-2],\"qux\":[2]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just 2, baz = [2,-2], qux = Just [2]}))))
  , test "3" (\_ -> equalHack "{\"foo\":-4,\"bar\":-1,\"baz\":[2,-3,1],\"qux\":[-2,4,4,-4]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -4, bar = Just (-1), baz = [2,-3,1], qux = Just [-2,4,4,-4]}))))
  , test "4" (\_ -> equalHack "{\"foo\":0,\"bar\":4,\"baz\":[1,-6,-3,-6,3,0],\"qux\":[0,6,2,1,2]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just 4, baz = [1,-6,-3,-6,3,0], qux = Just [0,6,2,1,2]}))))
  , test "5" (\_ -> equalHack "{\"foo\":6,\"bar\":-2,\"baz\":[-6,-6,-7,-2],\"qux\":[7,-1]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 6, bar = Just (-2), baz = [-6,-6,-7,-2], qux = Just [7,-1]}))))
  , test "6" (\_ -> equalHack "{\"foo\":-3,\"bar\":8,\"baz\":[2,6,1,-4,6],\"qux\":[10,2,-7,2,10,-7,9,7,-10]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -3, bar = Just 8, baz = [2,6,1,-4,6], qux = Just [10,2,-7,2,10,-7,9,7,-10]}))))
  , test "7" (\_ -> equalHack "{\"foo\":9,\"bar\":11,\"baz\":[-5,5,0,-3,-1,4,-1,-1,8,-8],\"qux\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 9, bar = Just 11, baz = [-5,5,0,-3,-1,4,-1,-1,8,-8], qux = Just []}))))
  , test "8" (\_ -> equalHack "{\"foo\":-7,\"bar\":-5,\"baz\":[-10,-13,-2,-13],\"qux\":[3,-8,12,-2,-14]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -7, bar = Just (-5), baz = [-10,-13,-2,-13], qux = Just [3,-8,12,-2,-14]}))))
  , test "9" (\_ -> equalHack "{\"foo\":11,\"bar\":-9,\"baz\":[-6,5,5,8,-3,1,-2,5,-4,-7,2,-2,-10,0,-1,-5],\"qux\":[-14,3,-15,-13,-6,-16]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 11, bar = Just (-9), baz = [-6,5,5,8,-3,1,-2,5,-4,-7,2,-2,-10,0,-1,-5], qux = Just [-14,3,-15,-13,-6,-16]}))))
  , test "10" (\_ -> equalHack "{\"foo\":8,\"bar\":-13,\"baz\":[5,2,-16,1,8,-14,3,-10,-11,-16],\"qux\":[-1,1,16,-3,9,-11,7,5,-14,1,-2,-10,9,7,-7]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 8, bar = Just (-13), baz = [5,2,-16,1,8,-14,3,-10,-11,-16], qux = Just [-1,1,16,-3,9,-11,7,5,-14,1,-2,-10,9,7,-7]}))))
  , test "11" (\_ -> equalHack "{\"foo\":-19,\"bar\":18,\"baz\":[8,14,4,2,5,11,12,-18,12],\"qux\":[-10,-16,-4,1,4,13,3,13,8,16,15,-13,11,6,19]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -19, bar = Just 18, baz = [8,14,4,2,5,11,12,-18,12], qux = Just [-10,-16,-4,1,4,13,3,13,8,16,15,-13,11,6,19]}))))
  ]

recordDecodeNestTuple : Test
recordDecodeNestTuple = describe "Record decode NestTuple"
  [ test "1" (\_ -> equal (Ok (RecordNestTuple ([],([],[])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[],[[],[]]]"))
  , test "2" (\_ -> equal (Ok (RecordNestTuple ([],([0,-2],[0,-1])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[],[[0,-2],[0,-1]]]"))
  , test "3" (\_ -> equal (Ok (RecordNestTuple ([1,-1,1],([-3,-4,1],[-3,-3,-4,-3])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[1,-1,1],[[-3,-4,1],[-3,-3,-4,-3]]]"))
  , test "4" (\_ -> equal (Ok (RecordNestTuple ([2,1,5],([3,5,-3,-5,-1,-5],[-4,-2,3,6,-2])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[2,1,5],[[3,5,-3,-5,-1,-5],[-4,-2,3,6,-2]]]"))
  , test "5" (\_ -> equal (Ok (RecordNestTuple ([-5,5,4,7,5,-8],([7,5,7,8,-7],[6,2])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-5,5,4,7,5,-8],[[7,5,7,8,-7],[6,2]]]"))
  , test "6" (\_ -> equal (Ok (RecordNestTuple ([4,-2],([8],[])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[4,-2],[[8],[]]]"))
  , test "7" (\_ -> equal (Ok (RecordNestTuple ([-2,0,-8,0,-4],([1,-7,-3,3],[0,3,-12,7])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-2,0,-8,0,-4],[[1,-7,-3,3],[0,3,-12,7]]]"))
  , test "8" (\_ -> equal (Ok (RecordNestTuple ([-7,-10,4,4,-1],([13,10,-10,9,-2],[])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-7,-10,4,4,-1],[[13,10,-10,9,-2],[]]]"))
  , test "9" (\_ -> equal (Ok (RecordNestTuple ([16,9,2,-7,15,-16,13,-2,12,-1],([16,12,-16,-13,-2,2,-16,6,-8,0,2,10,-5,-2,2],[11,-16,-10,-9,-5,-16,-16,-15,-8])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[16,9,2,-7,15,-16,13,-2,12,-1],[[16,12,-16,-13,-2,2,-16,6,-8,0,2,10,-5,-2,2],[11,-16,-10,-9,-5,-16,-16,-15,-8]]]"))
  , test "10" (\_ -> equal (Ok (RecordNestTuple ([3,-7,-16,16,-11,-8],([4,6,-18,-4,4,-13,-2,-13,-2],[7,-14,-8,-4,-15,-14,6,-6,-12,6,-8,18])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[3,-7,-16,16,-11,-8],[[4,6,-18,-4,4,-13,-2,-13,-2],[7,-14,-8,-4,-15,-14,6,-6,-12,6,-8,18]]]"))
  , test "11" (\_ -> equal (Ok (RecordNestTuple ([4,-14,-15,-5,14,2,-13,-11,6,16,-14,-7,15,1,-10,13,17,13],([13,10,-12],[-15,9,-11,-18,-14,3,-12,0])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[4,-14,-15,-5,14,2,-13,-11,6,16,-14,-7,15,1,-10,13,17,13],[[13,10,-12],[-15,9,-11,-18,-14,3,-12,0]]]"))
  ]

recordEncodeNestTuple : Test
recordEncodeNestTuple = describe "Record encode NestTuple"
  [ test "1" (\_ -> equalHack "[[],[[],[]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([],([],[]))))))
  , test "2" (\_ -> equalHack "[[],[[0,-2],[0,-1]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([],([0,-2],[0,-1]))))))
  , test "3" (\_ -> equalHack "[[1,-1,1],[[-3,-4,1],[-3,-3,-4,-3]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([1,-1,1],([-3,-4,1],[-3,-3,-4,-3]))))))
  , test "4" (\_ -> equalHack "[[2,1,5],[[3,5,-3,-5,-1,-5],[-4,-2,3,6,-2]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([2,1,5],([3,5,-3,-5,-1,-5],[-4,-2,3,6,-2]))))))
  , test "5" (\_ -> equalHack "[[-5,5,4,7,5,-8],[[7,5,7,8,-7],[6,2]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-5,5,4,7,5,-8],([7,5,7,8,-7],[6,2]))))))
  , test "6" (\_ -> equalHack "[[4,-2],[[8],[]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([4,-2],([8],[]))))))
  , test "7" (\_ -> equalHack "[[-2,0,-8,0,-4],[[1,-7,-3,3],[0,3,-12,7]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-2,0,-8,0,-4],([1,-7,-3,3],[0,3,-12,7]))))))
  , test "8" (\_ -> equalHack "[[-7,-10,4,4,-1],[[13,10,-10,9,-2],[]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-7,-10,4,4,-1],([13,10,-10,9,-2],[]))))))
  , test "9" (\_ -> equalHack "[[16,9,2,-7,15,-16,13,-2,12,-1],[[16,12,-16,-13,-2,2,-16,6,-8,0,2,10,-5,-2,2],[11,-16,-10,-9,-5,-16,-16,-15,-8]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([16,9,2,-7,15,-16,13,-2,12,-1],([16,12,-16,-13,-2,2,-16,6,-8,0,2,10,-5,-2,2],[11,-16,-10,-9,-5,-16,-16,-15,-8]))))))
  , test "10" (\_ -> equalHack "[[3,-7,-16,16,-11,-8],[[4,6,-18,-4,4,-13,-2,-13,-2],[7,-14,-8,-4,-15,-14,6,-6,-12,6,-8,18]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([3,-7,-16,16,-11,-8],([4,6,-18,-4,4,-13,-2,-13,-2],[7,-14,-8,-4,-15,-14,6,-6,-12,6,-8,18]))))))
  , test "11" (\_ -> equalHack "[[4,-14,-15,-5,14,2,-13,-11,6,16,-14,-7,15,1,-10,13,17,13],[[13,10,-12],[-15,9,-11,-18,-14,3,-12,0]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([4,-14,-15,-5,14,2,-13,-11,6,16,-14,-7,15,1,-10,13,17,13],([13,10,-12],[-15,9,-11,-18,-14,3,-12,0]))))))
  ]

simpleEncode01 : Test
simpleEncode01 = describe "Simple encode 01"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 []))))
  , test "2" (\_ -> equalHack "[-2]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-2]))))
  , test "3" (\_ -> equalHack "[-1,3,0,4]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-1,3,0,4]))))
  , test "4" (\_ -> equalHack "[2,-6]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [2,-6]))))
  , test "5" (\_ -> equalHack "[5,4,6,6,6,2]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [5,4,6,6,6,2]))))
  , test "6" (\_ -> equalHack "[-3,6,0]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-3,6,0]))))
  , test "7" (\_ -> equalHack "[-5,-6,9,12,-2,-1,-11]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-5,-6,9,12,-2,-1,-11]))))
  , test "8" (\_ -> equalHack "[-7,-6,12,12,13,6,-10,13,7]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-7,-6,12,12,13,6,-10,13,7]))))
  , test "9" (\_ -> equalHack "[-9,-16,5,-2]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-9,-16,5,-2]))))
  , test "10" (\_ -> equalHack "[18,-7,-5]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [18,-7,-5]))))
  , test "11" (\_ -> equalHack "[10,-17,-6,-11,12,4,20,13]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [10,-17,-6,-11,12,4,20,13]))))
  ]

simpleEncode02 : Test
simpleEncode02 = describe "Simple encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 []))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 []))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 []))))
  , test "4" (\_ -> equalHack "[4,-5,-4,-3,-4,2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [4,-5,-4,-3,-4,2]))))
  , test "5" (\_ -> equalHack "[-3,0,1,4,-3,-7]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-3,0,1,4,-3,-7]))))
  , test "6" (\_ -> equalHack "[10,-4,4,9,-8]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [10,-4,4,9,-8]))))
  , test "7" (\_ -> equalHack "[12,3,7,-11]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [12,3,7,-11]))))
  , test "8" (\_ -> equalHack "[7,-12,-7,-5,5,5,-2,-14,-6,3,10,-4,0,10]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [7,-12,-7,-5,5,5,-2,-14,-6,3,10,-4,0,10]))))
  , test "9" (\_ -> equalHack "[-3,5,6,4,-16,15,-1,-6,-7,-3]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-3,5,6,4,-16,15,-1,-6,-7,-3]))))
  , test "10" (\_ -> equalHack "[1,12,-18,11,1,10,-10,12,16,-14,11,18,2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [1,12,-18,11,1,10,-10,12,16,-14,11,18,2]))))
  , test "11" (\_ -> equalHack "[6,-2,13,4,-14,9,0,-17,20,-3,-11,-6,5,-19]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [6,-2,13,4,-14,9,0,-17,20,-3,-11,-6,5,-19]))))
  ]

simpleEncode03 : Test
simpleEncode03 = describe "Simple encode 03"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  , test "2" (\_ -> equalHack "[-2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-2]))))
  , test "3" (\_ -> equalHack "[0,1,-3,-3]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [0,1,-3,-3]))))
  , test "4" (\_ -> equalHack "[5]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [5]))))
  , test "5" (\_ -> equalHack "[0,5,7]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [0,5,7]))))
  , test "6" (\_ -> equalHack "[10,9,-9,7,5,10,4,3,-5,-1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [10,9,-9,7,5,10,4,3,-5,-1]))))
  , test "7" (\_ -> equalHack "[-10,10,-11,9,-7,-12,11,2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-10,10,-11,9,-7,-12,11,2]))))
  , test "8" (\_ -> equalHack "[-2,10,-11,8]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-2,10,-11,8]))))
  , test "9" (\_ -> equalHack "[4,5,13,-8,2,-11,-2,6,-15,2,2,16,11,-5]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [4,5,13,-8,2,-11,-2,6,-15,2,2,16,11,-5]))))
  , test "10" (\_ -> equalHack "[18,-3,2,7,-10,16,8,-6,7,-9,-18,-14,-2,10,-4,5,5,-7]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [18,-3,2,7,-10,16,8,-6,7,-9,-18,-14,-2,10,-4,5,5,-7]))))
  , test "11" (\_ -> equalHack "[-1,15,13,-20,19,12,-16,-1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-1,15,13,-20,19,12,-16,-1]))))
  ]

simpleEncode04 : Test
simpleEncode04 = describe "Simple encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "2" (\_ -> equalHack "[1]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [1]))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "4" (\_ -> equalHack "[1,2,5]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [1,2,5]))))
  , test "5" (\_ -> equalHack "[6,-1,8]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [6,-1,8]))))
  , test "6" (\_ -> equalHack "[-6,-8,2,0,-10,8,-3,-1,8]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-6,-8,2,0,-10,8,-3,-1,8]))))
  , test "7" (\_ -> equalHack "[-4,-3,3,12,-11,1,-7,4,-9,4,3,-5]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-4,-3,3,12,-11,1,-7,4,-9,4,3,-5]))))
  , test "8" (\_ -> equalHack "[4,-4,-3,8,8,0,-3,6,-4,9,-13,-12]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [4,-4,-3,8,8,0,-3,6,-4,9,-13,-12]))))
  , test "9" (\_ -> equalHack "[4,2,-14,-10,-11,4,-14,15]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [4,2,-14,-10,-11,4,-14,15]))))
  , test "10" (\_ -> equalHack "[6,-18,-9,-6,-15,-13,-4,-14,-13,-13,13,12,-17,-6,-14,7]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [6,-18,-9,-6,-15,-13,-4,-14,-13,-13,13,12,-17,-6,-14,7]))))
  , test "11" (\_ -> equalHack "[-9,-9,6,12,-17,-2,6,-20,-19,-18]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-9,-9,6,12,-17,-2,6,-20,-19,-18]))))
  ]

simpleDecode01 : Test
simpleDecode01 = describe "Simple decode 01"
  [ test "1" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple01 [-2])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-2]"))
  , test "3" (\_ -> equal (Ok (Simple01 [-1,3,0,4])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-1,3,0,4]"))
  , test "4" (\_ -> equal (Ok (Simple01 [2,-6])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[2,-6]"))
  , test "5" (\_ -> equal (Ok (Simple01 [5,4,6,6,6,2])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[5,4,6,6,6,2]"))
  , test "6" (\_ -> equal (Ok (Simple01 [-3,6,0])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-3,6,0]"))
  , test "7" (\_ -> equal (Ok (Simple01 [-5,-6,9,12,-2,-1,-11])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-5,-6,9,12,-2,-1,-11]"))
  , test "8" (\_ -> equal (Ok (Simple01 [-7,-6,12,12,13,6,-10,13,7])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-7,-6,12,12,13,6,-10,13,7]"))
  , test "9" (\_ -> equal (Ok (Simple01 [-9,-16,5,-2])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-9,-16,5,-2]"))
  , test "10" (\_ -> equal (Ok (Simple01 [18,-7,-5])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[18,-7,-5]"))
  , test "11" (\_ -> equal (Ok (Simple01 [10,-17,-6,-11,12,4,20,13])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[10,-17,-6,-11,12,4,20,13]"))
  ]

simpleDecode02 : Test
simpleDecode02 = describe "Simple decode 02"
  [ test "1" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "4" (\_ -> equal (Ok (Simple02 [4,-5,-4,-3,-4,2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[4,-5,-4,-3,-4,2]"))
  , test "5" (\_ -> equal (Ok (Simple02 [-3,0,1,4,-3,-7])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-3,0,1,4,-3,-7]"))
  , test "6" (\_ -> equal (Ok (Simple02 [10,-4,4,9,-8])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[10,-4,4,9,-8]"))
  , test "7" (\_ -> equal (Ok (Simple02 [12,3,7,-11])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[12,3,7,-11]"))
  , test "8" (\_ -> equal (Ok (Simple02 [7,-12,-7,-5,5,5,-2,-14,-6,3,10,-4,0,10])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[7,-12,-7,-5,5,5,-2,-14,-6,3,10,-4,0,10]"))
  , test "9" (\_ -> equal (Ok (Simple02 [-3,5,6,4,-16,15,-1,-6,-7,-3])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-3,5,6,4,-16,15,-1,-6,-7,-3]"))
  , test "10" (\_ -> equal (Ok (Simple02 [1,12,-18,11,1,10,-10,12,16,-14,11,18,2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[1,12,-18,11,1,10,-10,12,16,-14,11,18,2]"))
  , test "11" (\_ -> equal (Ok (Simple02 [6,-2,13,4,-14,9,0,-17,20,-3,-11,-6,5,-19])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[6,-2,13,4,-14,9,0,-17,20,-3,-11,-6,5,-19]"))
  ]

simpleDecode03 : Test
simpleDecode03 = describe "Simple decode 03"
  [ test "1" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple03 [-2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-2]"))
  , test "3" (\_ -> equal (Ok (Simple03 [0,1,-3,-3])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[0,1,-3,-3]"))
  , test "4" (\_ -> equal (Ok (Simple03 [5])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[5]"))
  , test "5" (\_ -> equal (Ok (Simple03 [0,5,7])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[0,5,7]"))
  , test "6" (\_ -> equal (Ok (Simple03 [10,9,-9,7,5,10,4,3,-5,-1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[10,9,-9,7,5,10,4,3,-5,-1]"))
  , test "7" (\_ -> equal (Ok (Simple03 [-10,10,-11,9,-7,-12,11,2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-10,10,-11,9,-7,-12,11,2]"))
  , test "8" (\_ -> equal (Ok (Simple03 [-2,10,-11,8])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-2,10,-11,8]"))
  , test "9" (\_ -> equal (Ok (Simple03 [4,5,13,-8,2,-11,-2,6,-15,2,2,16,11,-5])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[4,5,13,-8,2,-11,-2,6,-15,2,2,16,11,-5]"))
  , test "10" (\_ -> equal (Ok (Simple03 [18,-3,2,7,-10,16,8,-6,7,-9,-18,-14,-2,10,-4,5,5,-7])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[18,-3,2,7,-10,16,8,-6,7,-9,-18,-14,-2,10,-4,5,5,-7]"))
  , test "11" (\_ -> equal (Ok (Simple03 [-1,15,13,-20,19,12,-16,-1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-1,15,13,-20,19,12,-16,-1]"))
  ]

simpleDecode04 : Test
simpleDecode04 = describe "Simple decode 04"
  [ test "1" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple04 [1])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[1]"))
  , test "3" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "4" (\_ -> equal (Ok (Simple04 [1,2,5])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[1,2,5]"))
  , test "5" (\_ -> equal (Ok (Simple04 [6,-1,8])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[6,-1,8]"))
  , test "6" (\_ -> equal (Ok (Simple04 [-6,-8,2,0,-10,8,-3,-1,8])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-6,-8,2,0,-10,8,-3,-1,8]"))
  , test "7" (\_ -> equal (Ok (Simple04 [-4,-3,3,12,-11,1,-7,4,-9,4,3,-5])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-4,-3,3,12,-11,1,-7,4,-9,4,3,-5]"))
  , test "8" (\_ -> equal (Ok (Simple04 [4,-4,-3,8,8,0,-3,6,-4,9,-13,-12])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[4,-4,-3,8,8,0,-3,6,-4,9,-13,-12]"))
  , test "9" (\_ -> equal (Ok (Simple04 [4,2,-14,-10,-11,4,-14,15])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[4,2,-14,-10,-11,4,-14,15]"))
  , test "10" (\_ -> equal (Ok (Simple04 [6,-18,-9,-6,-15,-13,-4,-14,-13,-13,13,12,-17,-6,-14,7])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[6,-18,-9,-6,-15,-13,-4,-14,-13,-13,13,12,-17,-6,-14,7]"))
  , test "11" (\_ -> equal (Ok (Simple04 [-9,-9,6,12,-17,-2,6,-20,-19,-18])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-9,-9,6,12,-17,-2,6,-20,-19,-18]"))
  ]

simplerecordEncode01 : Test
simplerecordEncode01 = describe "SimpleRecord encode 01"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "3" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "4" (\_ -> equalHack "{\"qux\":[0,3,-3,1,-4,-5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [0,3,-3,1,-4,-5]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "6" (\_ -> equalHack "{\"qux\":[-3,-4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-3,-4]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[-4,2,-3]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-4,2,-3]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[10]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [10]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[-16,1,8,-2,5,13,1,12,-11]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-16,1,8,-2,5,13,1,12,-11]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[-14,-16,7,-1,11]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-14,-16,7,-1,11]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[7,-1,1,15,-15,19,-7,11,-7,3,18,8,13,0,3]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [7,-1,1,15,-15,19,-7,11,-7,3,18,8,13,0,3]}))))
  ]

simplerecordEncode02 : Test
simplerecordEncode02 = describe "SimpleRecord encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "3" (\_ -> equalHack "[-3,0]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-3,0]}))))
  , test "4" (\_ -> equalHack "[0,6]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [0,6]}))))
  , test "5" (\_ -> equalHack "[8,-5,1,-7]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [8,-5,1,-7]}))))
  , test "6" (\_ -> equalHack "[-9,5,-8,-6,1,-5,9,5]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-9,5,-8,-6,1,-5,9,5]}))))
  , test "7" (\_ -> equalHack "[6,7,-2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [6,7,-2]}))))
  , test "8" (\_ -> equalHack "[-3,2,4,-5,0,-10,-10,-3,-10,9,0,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-3,2,4,-5,0,-10,-10,-3,-10,9,0,-4]}))))
  , test "9" (\_ -> equalHack "[0,12,11,-9,9,2,-5]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [0,12,11,-9,9,2,-5]}))))
  , test "10" (\_ -> equalHack "[-18,14,2,-17,4,0,-14,-3,18,9,-10,-8,5,10,-12,9]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-18,14,2,-17,4,0,-14,-3,18,9,-10,-8,5,10,-12,9]}))))
  , test "11" (\_ -> equalHack "[12,-3,10,15,-12,20,17,-5,14,9]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [12,-3,10,15,-12,20,17,-5,14,9]}))))
  ]

simplerecordEncode03 : Test
simplerecordEncode03 = describe "SimpleRecord encode 03"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "3" (\_ -> equalHack "{\"qux\":[-3,4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-3,4]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[-1,4,1,4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-1,4,1,4]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[0,3]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [0,3]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[4,-6,-10,4,0,8,4,8,-6]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [4,-6,-10,4,0,8,4,8,-6]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[5,10,-8,3,-12,6,9]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [5,10,-8,3,-12,6,9]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[13,4,-7,-14,12,-13,-7,0,-5,-9,14,-10,6]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [13,4,-7,-14,12,-13,-7,0,-5,-9,14,-10,6]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[6,-6,-3,2,4,-8,2,13,13,5,7,1,16,15,-7]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [6,-6,-3,2,4,-8,2,13,13,5,7,1,16,15,-7]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[-5,1,-18,-18,4,8,3,1,2,8]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-5,1,-18,-18,4,8,3,1,2,8]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[17,-19,15,17]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [17,-19,15,17]}))))
  ]

simplerecordEncode04 : Test
simplerecordEncode04 = describe "SimpleRecord encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "2" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [2]}))))
  , test "3" (\_ -> equalHack "[4,1,3]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [4,1,3]}))))
  , test "4" (\_ -> equalHack "[-2,-2]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-2,-2]}))))
  , test "5" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "6" (\_ -> equalHack "[9,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [9,-4]}))))
  , test "7" (\_ -> equalHack "[6]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [6]}))))
  , test "8" (\_ -> equalHack "[-5,14,-6,-3,-12]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-5,14,-6,-3,-12]}))))
  , test "9" (\_ -> equalHack "[12,-11,-4,-3,-6,-7,-11,2,16,13,-10,-4,-11,8,-7]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [12,-11,-4,-3,-6,-7,-11,2,16,13,-10,-4,-11,8,-7]}))))
  , test "10" (\_ -> equalHack "[-4,14,-15,2,-9,12,-8,12,-11,-1,-11,-18,-13]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-4,14,-15,2,-9,12,-8,12,-11,-1,-11,-18,-13]}))))
  , test "11" (\_ -> equalHack "[7,-9,10,-17,-5,11,-6]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [7,-9,10,-17,-5,11,-6]}))))
  ]

simplerecordDecode01 : Test
simplerecordDecode01 = describe "SimpleRecord decode 01"
  [ test "1" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord01 {qux = [0,3,-3,1,-4,-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0,3,-3,1,-4,-5]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord01 {qux = [-3,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-3,-4]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord01 {qux = [-4,2,-3]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-4,2,-3]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord01 {qux = [10]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[10]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord01 {qux = [-16,1,8,-2,5,13,1,12,-11]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-16,1,8,-2,5,13,1,12,-11]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord01 {qux = [-14,-16,7,-1,11]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-14,-16,7,-1,11]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord01 {qux = [7,-1,1,15,-15,19,-7,11,-7,3,18,8,13,0,3]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[7,-1,1,15,-15,19,-7,11,-7,3,18,8,13,0,3]}"))
  ]

simplerecordDecode02 : Test
simplerecordDecode02 = describe "SimpleRecord decode 02"
  [ test "1" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord02 {qux = [-3,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-3,0]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord02 {qux = [0,6]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[0,6]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord02 {qux = [8,-5,1,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[8,-5,1,-7]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord02 {qux = [-9,5,-8,-6,1,-5,9,5]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-9,5,-8,-6,1,-5,9,5]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord02 {qux = [6,7,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[6,7,-2]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord02 {qux = [-3,2,4,-5,0,-10,-10,-3,-10,9,0,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-3,2,4,-5,0,-10,-10,-3,-10,9,0,-4]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord02 {qux = [0,12,11,-9,9,2,-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[0,12,11,-9,9,2,-5]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord02 {qux = [-18,14,2,-17,4,0,-14,-3,18,9,-10,-8,5,10,-12,9]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-18,14,2,-17,4,0,-14,-3,18,9,-10,-8,5,10,-12,9]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord02 {qux = [12,-3,10,15,-12,20,17,-5,14,9]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[12,-3,10,15,-12,20,17,-5,14,9]"))
  ]

simplerecordDecode03 : Test
simplerecordDecode03 = describe "SimpleRecord decode 03"
  [ test "1" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord03 {qux = [-3,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-3,4]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord03 {qux = [-1,4,1,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-1,4,1,4]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord03 {qux = [0,3]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0,3]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord03 {qux = [4,-6,-10,4,0,8,4,8,-6]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[4,-6,-10,4,0,8,4,8,-6]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord03 {qux = [5,10,-8,3,-12,6,9]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[5,10,-8,3,-12,6,9]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord03 {qux = [13,4,-7,-14,12,-13,-7,0,-5,-9,14,-10,6]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[13,4,-7,-14,12,-13,-7,0,-5,-9,14,-10,6]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord03 {qux = [6,-6,-3,2,4,-8,2,13,13,5,7,1,16,15,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[6,-6,-3,2,4,-8,2,13,13,5,7,1,16,15,-7]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord03 {qux = [-5,1,-18,-18,4,8,3,1,2,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-5,1,-18,-18,4,8,3,1,2,8]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord03 {qux = [17,-19,15,17]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[17,-19,15,17]}"))
  ]

simplerecordDecode04 : Test
simplerecordDecode04 = describe "SimpleRecord decode 04"
  [ test "1" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord04 {qux = [2]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord04 {qux = [4,1,3]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[4,1,3]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord04 {qux = [-2,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-2,-2]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord04 {qux = [9,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[9,-4]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord04 {qux = [6]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[6]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord04 {qux = [-5,14,-6,-3,-12]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-5,14,-6,-3,-12]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord04 {qux = [12,-11,-4,-3,-6,-7,-11,2,16,13,-10,-4,-11,8,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[12,-11,-4,-3,-6,-7,-11,2,16,13,-10,-4,-11,8,-7]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord04 {qux = [-4,14,-15,2,-9,12,-8,12,-11,-1,-11,-18,-13]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-4,14,-15,2,-9,12,-8,12,-11,-1,-11,-18,-13]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord04 {qux = [7,-9,10,-17,-5,11,-6]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[7,-9,10,-17,-5,11,-6]"))
  ]

sumEncodeUntagged : Test
sumEncodeUntagged = describe "Sum encode Untagged"
  [ test "1" (\_ -> equalHack "0"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 0))))
  , test "2" (\_ -> equalHack "0"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 0))))
  , test "3" (\_ -> equalHack "[0,-1,3,4]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [0,-1,3,4]))))
  , test "4" (\_ -> equalHack "-4"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-4)))))
  , test "5" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList []))))
  , test "6" (\_ -> equalHack "[3,7,-4,3,8,3,-9,4,-2,4]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [3,7,-4,3,8,3,-9,4,-2,4]))))
  , test "7" (\_ -> equalHack "11"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 11))))
  , test "8" (\_ -> equalHack "[5,-5,-13]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [5,-5,-13]))))
  , test "9" (\_ -> equalHack "[7,15,5,16]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [7,15,5,16]))))
  , test "10" (\_ -> equalHack "-11"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-11)))))
  , test "11" (\_ -> equalHack "12"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 12))))
  ]

sumDecodeUntagged : Test
sumDecodeUntagged = describe "Sum decode Untagged"
  [ test "1" (\_ -> equal (Ok (SMInt 0)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "0"))
  , test "2" (\_ -> equal (Ok (SMInt 0)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "0"))
  , test "3" (\_ -> equal (Ok (SMList [0,-1,3,4])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[0,-1,3,4]"))
  , test "4" (\_ -> equal (Ok (SMInt (-4))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-4"))
  , test "5" (\_ -> equal (Ok (SMList [])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[]"))
  , test "6" (\_ -> equal (Ok (SMList [3,7,-4,3,8,3,-9,4,-2,4])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[3,7,-4,3,8,3,-9,4,-2,4]"))
  , test "7" (\_ -> equal (Ok (SMInt 11)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "11"))
  , test "8" (\_ -> equal (Ok (SMList [5,-5,-13])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[5,-5,-13]"))
  , test "9" (\_ -> equal (Ok (SMList [7,15,5,16])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[7,15,5,16]"))
  , test "10" (\_ -> equal (Ok (SMInt (-11))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-11"))
  , test "11" (\_ -> equal (Ok (SMInt 12)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "12"))
  ]

sumEncodeIncludeUnit : Test
sumEncodeIncludeUnit = describe "Sum encode IncludeUnit"
  [ test "1" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [] []))))
  , test "3" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitZero\"}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitZero))))
  , test "4" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne []))))
  , test "5" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-3,-1,-8,4]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-3,-1,-8,4]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-7,6,-6]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-7,6,-6]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-7,-1,12,-8,-10]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-7,-1,12,-8,-10]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[9,13,6,-12],[-7,6,-3,-3,-12,4,-2,-3,-10,-10,5]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [9,13,6,-12] [-7,6,-3,-3,-12,4,-2,-3,-10,-10,5]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[14,16,-8,0,7,-16,-15,8,11,-12,1,16,8]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [14,16,-8,0,7,-16,-15,8,11,-12,1,16,8]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[-11,9],[-8,-2,-2,-12,-7,-11,9,-15,-14,-5,12,-9,5,10,14,-1,11]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [-11,9] [-8,-2,-2,-12,-7,-11,9,-15,-14,-5,12,-9,5,10,14,-1,11]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitZero\"}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitZero))))
  ]

sumDecodeIncludeUnit : Test
sumDecodeIncludeUnit = describe "Sum decode IncludeUnit"
  [ test "1" (\_ -> equal (Ok (SumIncludeUnitOne [])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[]}"))
  , test "2" (\_ -> equal (Ok (SumIncludeUnitTwo [] [])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[],[]]}"))
  , test "3" (\_ -> equal (Ok (SumIncludeUnitZero)) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitZero\"}"))
  , test "4" (\_ -> equal (Ok (SumIncludeUnitOne [])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[]}"))
  , test "5" (\_ -> equal (Ok (SumIncludeUnitOne [-3,-1,-8,4])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-3,-1,-8,4]}"))
  , test "6" (\_ -> equal (Ok (SumIncludeUnitOne [-7,6,-6])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-7,6,-6]}"))
  , test "7" (\_ -> equal (Ok (SumIncludeUnitOne [-7,-1,12,-8,-10])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-7,-1,12,-8,-10]}"))
  , test "8" (\_ -> equal (Ok (SumIncludeUnitTwo [9,13,6,-12] [-7,6,-3,-3,-12,4,-2,-3,-10,-10,5])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[9,13,6,-12],[-7,6,-3,-3,-12,4,-2,-3,-10,-10,5]]}"))
  , test "9" (\_ -> equal (Ok (SumIncludeUnitOne [14,16,-8,0,7,-16,-15,8,11,-12,1,16,8])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[14,16,-8,0,7,-16,-15,8,11,-12,1,16,8]}"))
  , test "10" (\_ -> equal (Ok (SumIncludeUnitTwo [-11,9] [-8,-2,-2,-12,-7,-11,9,-15,-14,-5,12,-9,5,10,14,-1,11])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[-11,9],[-8,-2,-2,-12,-7,-11,9,-15,-14,-5,12,-9,5,10,14,-1,11]]}"))
  , test "11" (\_ -> equal (Ok (SumIncludeUnitZero)) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitZero\"}"))
  ]

ntDecode1 : Test
ntDecode1 = describe "NT decode 1"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "2" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "3" (\_ -> equal (Ok ([-1,-4,2])) (Json.Decode.decodeString jsonDecNT1 "[-1,-4,2]"))
  , test "4" (\_ -> equal (Ok ([-4,-4,-4])) (Json.Decode.decodeString jsonDecNT1 "[-4,-4,-4]"))
  , test "5" (\_ -> equal (Ok ([0,-7,4,-6,-5,-6,3,-5])) (Json.Decode.decodeString jsonDecNT1 "[0,-7,4,-6,-5,-6,3,-5]"))
  , test "6" (\_ -> equal (Ok ([3,10,-2,-4,3])) (Json.Decode.decodeString jsonDecNT1 "[3,10,-2,-4,3]"))
  , test "7" (\_ -> equal (Ok ([-11,9,-6,7,8])) (Json.Decode.decodeString jsonDecNT1 "[-11,9,-6,7,8]"))
  , test "8" (\_ -> equal (Ok ([3,14,-5,-13,-8,-7,-6,-7,7,-2,-10,-9])) (Json.Decode.decodeString jsonDecNT1 "[3,14,-5,-13,-8,-7,-6,-7,7,-2,-10,-9]"))
  , test "9" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "10" (\_ -> equal (Ok ([8,16,3,6,8,-13,-8])) (Json.Decode.decodeString jsonDecNT1 "[8,16,3,6,8,-13,-8]"))
  , test "11" (\_ -> equal (Ok ([9,12,-6,-14,-6,-11,-17,-14,18,20])) (Json.Decode.decodeString jsonDecNT1 "[9,12,-6,-14,-6,-11,-17,-14,18,20]"))
  ]

ntEncode1 : Test
ntEncode1 = describe "NT encode 1"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "3" (\_ -> equalHack "[-1,-4,2]"(Json.Encode.encode 0 (jsonEncNT1 ([-1,-4,2]))))
  , test "4" (\_ -> equalHack "[-4,-4,-4]"(Json.Encode.encode 0 (jsonEncNT1 ([-4,-4,-4]))))
  , test "5" (\_ -> equalHack "[0,-7,4,-6,-5,-6,3,-5]"(Json.Encode.encode 0 (jsonEncNT1 ([0,-7,4,-6,-5,-6,3,-5]))))
  , test "6" (\_ -> equalHack "[3,10,-2,-4,3]"(Json.Encode.encode 0 (jsonEncNT1 ([3,10,-2,-4,3]))))
  , test "7" (\_ -> equalHack "[-11,9,-6,7,8]"(Json.Encode.encode 0 (jsonEncNT1 ([-11,9,-6,7,8]))))
  , test "8" (\_ -> equalHack "[3,14,-5,-13,-8,-7,-6,-7,7,-2,-10,-9]"(Json.Encode.encode 0 (jsonEncNT1 ([3,14,-5,-13,-8,-7,-6,-7,7,-2,-10,-9]))))
  , test "9" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "10" (\_ -> equalHack "[8,16,3,6,8,-13,-8]"(Json.Encode.encode 0 (jsonEncNT1 ([8,16,3,6,8,-13,-8]))))
  , test "11" (\_ -> equalHack "[9,12,-6,-14,-6,-11,-17,-14,18,20]"(Json.Encode.encode 0 (jsonEncNT1 ([9,12,-6,-14,-6,-11,-17,-14,18,20]))))
  ]

ntDecode2 : Test
ntDecode2 = describe "NT decode 2"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "2" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "3" (\_ -> equal (Ok ([1,3])) (Json.Decode.decodeString jsonDecNT2 "[1,3]"))
  , test "4" (\_ -> equal (Ok ([-2,-6,6])) (Json.Decode.decodeString jsonDecNT2 "[-2,-6,6]"))
  , test "5" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "6" (\_ -> equal (Ok ([-5,2,-1,6,4,-6,4,-7,-5])) (Json.Decode.decodeString jsonDecNT2 "[-5,2,-1,6,4,-6,4,-7,-5]"))
  , test "7" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "8" (\_ -> equal (Ok ([-6,-6,7,-10])) (Json.Decode.decodeString jsonDecNT2 "[-6,-6,7,-10]"))
  , test "9" (\_ -> equal (Ok ([-4,12,-13])) (Json.Decode.decodeString jsonDecNT2 "[-4,12,-13]"))
  , test "10" (\_ -> equal (Ok ([-4,-3,-7,6,5,2,-10,-4,5,-15,-9,8,1,0,14,-1])) (Json.Decode.decodeString jsonDecNT2 "[-4,-3,-7,6,5,2,-10,-4,5,-15,-9,8,1,0,14,-1]"))
  , test "11" (\_ -> equal (Ok ([5,-8,-10,-10,-13,-6])) (Json.Decode.decodeString jsonDecNT2 "[5,-8,-10,-10,-13,-6]"))
  ]

ntEncode2 : Test
ntEncode2 = describe "NT encode 2"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "3" (\_ -> equalHack "[1,3]"(Json.Encode.encode 0 (jsonEncNT2 ([1,3]))))
  , test "4" (\_ -> equalHack "[-2,-6,6]"(Json.Encode.encode 0 (jsonEncNT2 ([-2,-6,6]))))
  , test "5" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "6" (\_ -> equalHack "[-5,2,-1,6,4,-6,4,-7,-5]"(Json.Encode.encode 0 (jsonEncNT2 ([-5,2,-1,6,4,-6,4,-7,-5]))))
  , test "7" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "8" (\_ -> equalHack "[-6,-6,7,-10]"(Json.Encode.encode 0 (jsonEncNT2 ([-6,-6,7,-10]))))
  , test "9" (\_ -> equalHack "[-4,12,-13]"(Json.Encode.encode 0 (jsonEncNT2 ([-4,12,-13]))))
  , test "10" (\_ -> equalHack "[-4,-3,-7,6,5,2,-10,-4,5,-15,-9,8,1,0,14,-1]"(Json.Encode.encode 0 (jsonEncNT2 ([-4,-3,-7,6,5,2,-10,-4,5,-15,-9,8,1,0,14,-1]))))
  , test "11" (\_ -> equalHack "[5,-8,-10,-10,-13,-6]"(Json.Encode.encode 0 (jsonEncNT2 ([5,-8,-10,-10,-13,-6]))))
  ]

ntDecode3 : Test
ntDecode3 = describe "NT decode 3"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "2" (\_ -> equal (Ok ([-1,2])) (Json.Decode.decodeString jsonDecNT3 "[-1,2]"))
  , test "3" (\_ -> equal (Ok ([-3,-3])) (Json.Decode.decodeString jsonDecNT3 "[-3,-3]"))
  , test "4" (\_ -> equal (Ok ([3,-5,0])) (Json.Decode.decodeString jsonDecNT3 "[3,-5,0]"))
  , test "5" (\_ -> equal (Ok ([7,1,4,6])) (Json.Decode.decodeString jsonDecNT3 "[7,1,4,6]"))
  , test "6" (\_ -> equal (Ok ([10,-5,-5,-4,-7,3,-2,2])) (Json.Decode.decodeString jsonDecNT3 "[10,-5,-5,-4,-7,3,-2,2]"))
  , test "7" (\_ -> equal (Ok ([3,-2,-4])) (Json.Decode.decodeString jsonDecNT3 "[3,-2,-4]"))
  , test "8" (\_ -> equal (Ok ([-8,-4,-6,5,0,14,-3])) (Json.Decode.decodeString jsonDecNT3 "[-8,-4,-6,5,0,14,-3]"))
  , test "9" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "10" (\_ -> equal (Ok ([11,-2,0,-1,-16,8,-10,8,-13,-15,12,-10,-11,-13,-9,-1,13])) (Json.Decode.decodeString jsonDecNT3 "[11,-2,0,-1,-16,8,-10,8,-13,-15,12,-10,-11,-13,-9,-1,13]"))
  , test "11" (\_ -> equal (Ok ([20,20,-18,-3,-5,-6,-15,11,16,13,15,5,17,-7,14,-7,10,6,-13,18])) (Json.Decode.decodeString jsonDecNT3 "[20,20,-18,-3,-5,-6,-15,11,16,13,15,5,17,-7,14,-7,10,6,-13,18]"))
  ]

ntEncode3 : Test
ntEncode3 = describe "NT encode 3"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "2" (\_ -> equalHack "[-1,2]"(Json.Encode.encode 0 (jsonEncNT3 ([-1,2]))))
  , test "3" (\_ -> equalHack "[-3,-3]"(Json.Encode.encode 0 (jsonEncNT3 ([-3,-3]))))
  , test "4" (\_ -> equalHack "[3,-5,0]"(Json.Encode.encode 0 (jsonEncNT3 ([3,-5,0]))))
  , test "5" (\_ -> equalHack "[7,1,4,6]"(Json.Encode.encode 0 (jsonEncNT3 ([7,1,4,6]))))
  , test "6" (\_ -> equalHack "[10,-5,-5,-4,-7,3,-2,2]"(Json.Encode.encode 0 (jsonEncNT3 ([10,-5,-5,-4,-7,3,-2,2]))))
  , test "7" (\_ -> equalHack "[3,-2,-4]"(Json.Encode.encode 0 (jsonEncNT3 ([3,-2,-4]))))
  , test "8" (\_ -> equalHack "[-8,-4,-6,5,0,14,-3]"(Json.Encode.encode 0 (jsonEncNT3 ([-8,-4,-6,5,0,14,-3]))))
  , test "9" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "10" (\_ -> equalHack "[11,-2,0,-1,-16,8,-10,8,-13,-15,12,-10,-11,-13,-9,-1,13]"(Json.Encode.encode 0 (jsonEncNT3 ([11,-2,0,-1,-16,8,-10,8,-13,-15,12,-10,-11,-13,-9,-1,13]))))
  , test "11" (\_ -> equalHack "[20,20,-18,-3,-5,-6,-15,11,16,13,15,5,17,-7,14,-7,10,6,-13,18]"(Json.Encode.encode 0 (jsonEncNT3 ([20,20,-18,-3,-5,-6,-15,11,16,13,15,5,17,-7,14,-7,10,6,-13,18]))))
  ]

ntDecode4 : Test
ntDecode4 = describe "NT decode 4"
  [ test "1" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "3" (\_ -> equal (Ok (NT4 {foo = [-2]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-2]}"))
  , test "4" (\_ -> equal (Ok (NT4 {foo = [1,3,-3,0,-3]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[1,3,-3,0,-3]}"))
  , test "5" (\_ -> equal (Ok (NT4 {foo = [-5,-8,-1,5,-5,-6,-8]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-5,-8,-1,5,-5,-6,-8]}"))
  , test "6" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "7" (\_ -> equal (Ok (NT4 {foo = [0,-7,4,7,-7,-11,-5]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[0,-7,4,7,-7,-11,-5]}"))
  , test "8" (\_ -> equal (Ok (NT4 {foo = [10,1,-1,4,-4]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[10,1,-1,4,-4]}"))
  , test "9" (\_ -> equal (Ok (NT4 {foo = [-10,-11,-1,5,-11]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-10,-11,-1,5,-11]}"))
  , test "10" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "11" (\_ -> equal (Ok (NT4 {foo = [5,19,-15,-18,-15,-15,-15,15,6,11,4,-20,19,10]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[5,19,-15,-18,-15,-15,-15,15,6,11,4,-20,19,10]}"))
  ]

ntEncode4 : Test
ntEncode4 = describe "NT encode 4"
  [ test "1" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "2" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "3" (\_ -> equalHack "{\"foo\":[-2]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-2]}))))
  , test "4" (\_ -> equalHack "{\"foo\":[1,3,-3,0,-3]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [1,3,-3,0,-3]}))))
  , test "5" (\_ -> equalHack "{\"foo\":[-5,-8,-1,5,-5,-6,-8]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-5,-8,-1,5,-5,-6,-8]}))))
  , test "6" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "7" (\_ -> equalHack "{\"foo\":[0,-7,4,7,-7,-11,-5]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [0,-7,4,7,-7,-11,-5]}))))
  , test "8" (\_ -> equalHack "{\"foo\":[10,1,-1,4,-4]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [10,1,-1,4,-4]}))))
  , test "9" (\_ -> equalHack "{\"foo\":[-10,-11,-1,5,-11]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-10,-11,-1,5,-11]}))))
  , test "10" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "11" (\_ -> equalHack "{\"foo\":[5,19,-15,-18,-15,-15,-15,15,6,11,4,-20,19,10]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [5,19,-15,-18,-15,-15,-15,15,6,11,4,-20,19,10]}))))
  ]

