module Shuffle exposing (..)

import Array as Array exposing (Array)
import Card exposing (Card)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Pile exposing (Pile)

import Process
import Random as Random exposing (..)
import Task

--  ####
--  ####      Shuffling is inefficient because of the excessive use of Cmd Msg
--  ####


type alias Model =
    {
        shuffledTimes : Int
        , pile : Pile
        , shufflingDone : Bool
    }


shuffleTimesTodo : Int
shuffleTimesTodo = 30


init : Pile -> ( Model, Cmd Msg )
init pile =
    ( { shuffledTimes = 0, pile = pile, shufflingDone = False }, randomShuffle 0 )


-- ####
-- ####    VIEW
-- ####


view : Model -> Html msg
view model =
    div [ class "shuffle-container" ]
        ( Array.indexedMap viewShuffleCard model.pile |> Array.toList )


viewShuffleCard : Int -> Card -> Html msg
viewShuffleCard index card =
    if index == 0 then
        div [ class "card-shuffle-bottom" ]
            [ Card.view card ]
    else
        div [ class "card-shuffle" ]
            [ Card.view card ]

--  ####
--  ####      Update
--  ####


randomShuffle : Int -> Cmd Msg
randomShuffle turn =
    Process.sleep ( if turn == 0 then 1300 else 200 )
    --Process.sleep ( if turn == 0 then 0 else 0 )
        |> Task.perform
            (always DoRandom)


type Msg
    = Shuffle Int
    | DoRandom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoRandom ->
            ( model, Random.generate Shuffle (Random.int 1 (Array.length model.pile - 1) ) )

        Shuffle random ->
            let
                shuffledTimes1 = model.shuffledTimes + 1
                shufflingDone1 = ( shuffledTimes1 >= shuffleTimesTodo )
                pile1 = shuffleOverhand model.pile random
            in
            (
                {
                    shuffledTimes = shuffledTimes1
                    , pile = pile1
                    , shufflingDone = shufflingDone1
                }
                ,
                    if not shufflingDone1 then
                        randomShuffle shuffledTimes1
                    else
                        Cmd.none
            )


shuffleOverhand : Pile -> Int -> Pile
shuffleOverhand pile numberOfCards =
    Array.append
        (Array.slice numberOfCards (Array.length pile) pile)
        ( (Array.slice 0 numberOfCards pile) |> Array.toList |> List.reverse |> Array.fromList )
