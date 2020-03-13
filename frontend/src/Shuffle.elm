module Shuffle exposing (..)

import Array as Array exposing (Array)
import Pile exposing (Pile)

import Random as Random exposing (..)

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
shuffleTimesTodo = 66


init : Pile -> ( Model, Cmd Msg )
init pile =
    ( { shuffledTimes = 0, pile = pile, shufflingDone = False }, randomShuffle ( Array.length pile ) )


--  ####
--  ####      Update
--  ####


randomShuffle : Int -> Cmd Msg
randomShuffle pileLength =
    Random.generate Shuffle (Random.int 1 (pileLength - 1) )


type Msg
    = Shuffle Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        msgA = Debug.log "msg" msg
        shuffledTimesA = Debug.log "shuffledTimes" model.shuffledTimes
        shufflingDoneA = Debug.log "shuffled" model.shufflingDone
    in
    case msg of
        Shuffle random ->
            let
                shuffledTimes1 = model.shuffledTimes + 1
                shufflingDone1 = ( shuffledTimes1 >= shuffleTimesTodo )
                pile1 = Debug.log "pile1" ( shuffleOverhand model.pile random )
            in
            (
                {
                    shuffledTimes = shuffledTimes1
                    , pile = pile1
                    , shufflingDone = shufflingDone1
                }
                ,
                    if not shufflingDone1 then
                        randomShuffle ( Array.length pile1 )
                    else
                       Cmd.none
            )


shuffleOverhand : Pile -> Int -> Pile
shuffleOverhand pile numberOfCards =
    Array.append
        (Array.slice numberOfCards (Array.length pile) pile)
        ( (Array.slice 0 numberOfCards pile) |> Array.toList |> List.reverse |> Array.fromList )
