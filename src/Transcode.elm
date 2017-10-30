module Transcode exposing (..)

{-| Allow transcoding from one data-structure to another in a style similar to
that used in `elm-json-pipeline`.


# Basics

@docs Transcoder, run, transcodeTo


# Supplying values

@docs supply, supplyMaybe, supplyResult, hardcoded


# Combining and chaining

@docs map, andMap, map2, map3, map4, map5

-}


{-| A transcoder is really just a function `a -> Result String b`. The point is
to make it easy to pass this around in its unevaluated form and reason about it
as if it were a thing on its own, much like a JSON decoder.

In fact, one can think of a JSON `Decoder a` as a `Transcoder Value a`,
something I've [implemented]() with fancy Errors.

[implemented]: http://package.elm-lang.org/packages/zwilias/json-decode-exploration/3.0.0/Json-Decode-Exploration#Decoder

-}
type alias Transcoder a b =
    a -> Result String b


{-| Actually `run` a transcoder. Since a transcoder really just _is_ a function,
its definition is just `identity`.

    type alias Person = { name : String, age : String }
    type alias ProperPerson = { name : String, age : Int }


    toProperPerson : Transcoder { name : String, age : String } { name : String, age : Int }
    toProperPerson =
        transcodeTo (\name age -> { name = name, age = age })
            |> supply .name
            |> supplyResult (String.toInt << .age)


    { name = "Alice", age = "22" }
        |> run toProperPerson
    --> Ok { name = "Alice", age = 22 }

-}
run : Transcoder a b -> a -> Result String b
run =
    identity


{-| Start a transcoding pipeine. This is a convenient alias for `succeed`.
-}
transcodeTo : b -> Transcoder a b
transcodeTo =
    succeed


{-| Tansform the resulting value of a transcoder. This allows doing things like
wrapping a record in a union type.
-}
map : (a -> b) -> Transcoder i a -> Transcoder i b
map f transcoder =
    Result.map f << transcoder


{-| Combine the results of 2 transcoders using a function, ending up with
transcoder for the combined result.

    map2 (,) (succeed "a") (succeed "b")
        |> flip run ()
    --> Ok ( "a", "b" )

-}
map2 : (a -> b -> c) -> Transcoder i a -> Transcoder i b -> Transcoder i c
map2 f transA transB input =
    Result.map2 f (transA input) (transB input)


{-| Combine 3 transcoders.
-}
map3 :
    (a -> b -> c -> d)
    -> Transcoder i a
    -> Transcoder i b
    -> Transcoder i c
    -> Transcoder i d
map3 f transA transB transC input =
    Result.map3 f (transA input) (transB input) (transC input)


{-| Combine 4 transcoders.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Transcoder i a
    -> Transcoder i b
    -> Transcoder i c
    -> Transcoder i d
    -> Transcoder i e
map4 f transA transB transC transD input =
    Result.map4 f (transA input) (transB input) (transC input) (transD input)


{-| Combine 5 transcoders.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Transcoder i a
    -> Transcoder i b
    -> Transcoder i c
    -> Transcoder i d
    -> Transcoder i e
    -> Transcoder i f
map5 f transA transB transC transD transE input =
    Result.map5 f (transA input) (transB input) (transC input) (transD input) (transE input)


{-| Combine 6 transcoders.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Transcoder i a
    -> Transcoder i b
    -> Transcoder i c
    -> Transcoder i d
    -> Transcoder i e
    -> Transcoder i f
    -> Transcoder i g
map6 f transA transB transC transD transE transF =
    map5 f transA transB transC transD transE
        |> andMap transF


{-| Combine 7 transcoders.

To combine more, use the convenient, pipeline friendly `supply`, `supplyResult`,
`supplyMaybe` and `hardcoded` functions.

-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Transcoder i a
    -> Transcoder i b
    -> Transcoder i c
    -> Transcoder i d
    -> Transcoder i e
    -> Transcoder i f
    -> Transcoder i g
    -> Transcoder i h
map7 f transA transB transC transD transE transF transG =
    map5 f transA transB transC transD transE
        |> andMap transF
        |> andMap transG


{-| Allow chaining a transcoder that depends on the value produced by the
preceding transcoder.
-}
andThen : (a -> Transcoder i b) -> Transcoder i a -> Transcoder i b
andThen toTranscoderB transcoderA input =
    transcoderA input |> Result.andThen (flip toTranscoderB input)


{-| Insert a "data extraction function" in the transcoder.

For example, when going from `{ r | name : String }` to `{ r | uppercasedName : String }`
you could use `... |> supply (String.toUpper << .name)`.

When dealing with a `Maybe`, this can be used to provide a default value:
`... |> supply (Maybe.withDefault "fallback" << .name)`

-}
supply : (i -> a) -> Transcoder i (a -> b) -> Transcoder i b
supply f =
    andMap (Ok << f)


{-| Insert a hardcoded value in the transcoder. This allows adding hardcoded
information in the transcoded result that was not available in the input.
-}
hardcoded : a -> Transcoder i (a -> b) -> Transcoder i b
hardcoded val transcoder =
    transcoder |> andMap (succeed val)


{-| An alias for `transcodeTo`, useful when dealing with `mapN` or `andMap`.

Lifts any value into a transcoder that always succeeds with that value.

-}
succeed : a -> Transcoder i a
succeed =
    always << Ok


{-| Create a transcoder that always fails with a given string.
-}
fail : String -> Transcoder i a
fail =
    always << Err


{-| Insert the a function producing a `Result` (i.e. a transcoder) into the
pipeline.
-}
supplyResult : Transcoder i a -> Transcoder i (a -> b) -> Transcoder i b
supplyResult =
    andMap


{-| Insert a function producing a `Maybe` into the transcoder, with an error
message to use when the produced value is `Nothing`. If you don't want to unwrap
the `Maybe`, simply use `supply` instead.
-}
supplyMaybe : (i -> Maybe a) -> String -> Transcoder i (a -> b) -> Transcoder i b
supplyMaybe toMaybe error =
    andMap (toMaybe >> Maybe.map Ok >> Maybe.withDefault (Err error))


{-| The building block for `supply` and coincidentially an alias for
`supplyResult` (or, technically, the other way around).
-}
andMap : Transcoder i a -> Transcoder i (a -> b) -> Transcoder i b
andMap =
    map2 (|>)
