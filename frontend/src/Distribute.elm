module Distribute exposing (..)

import Array as Array exposing (Array)
import Card exposing (Card)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Pile exposing (Pile)

import Process
import Random as Random exposing (..)
import Task

--  ####
--  ####      Distruting is inefficient because of the excessive use of Cmd Msg
--  ####


type alias Model =
    {
        pile : Pile
        , piles : Array Pile
        , distributorCardIndex : Int
        , distributingDone : Bool
    }

init : Model
init =
     {
        pile = Array.empty
        , piles = Array.empty
        , distributorCardIndex = 0
        , distributingDone = False
     }


start : Pile -> ( Model, Cmd Msg )
start pile =
    let
        model =
            { pile = pile
            , piles = Array.repeat Pile.numberOfPiles Array.empty
            , distributorCardIndex = 0
            , distributingDone = False
            }
    in
        ( model , distribute 0 )


--  ####
--  ####      Update
--  ####


distribute : Int -> Cmd Msg
distribute index =
    Process.sleep ( if index == 0 then 800 else 80 )
        |> Task.perform
            ( always ( DoDistribute ) )


type Msg
    = DoDistribute


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    let
        { pile, piles, distributorCardIndex }  = model
        distributorPileIndex = modBy ( Array.length piles ) distributorCardIndex
        maybeCard = getCurrentCard pile distributorCardIndex
    in
        case maybeCard of
            Nothing ->
                (
                    { model
                    | distributingDone = True
                    }
                    , Cmd.none
                )

            Just card ->
                (
                    { model
                    | piles = distributeCard card piles distributorPileIndex
                    , distributorCardIndex = distributorCardIndex + 1
                    }
                    , distribute (distributorCardIndex + 1)
                )


getCurrentCard : Array Card -> Int -> Maybe Card
getCurrentCard cards index =
    Array.get index cards


distributeCard : Card -> Array Pile -> Int -> Array Pile
distributeCard card piles pileIndex =
    case Array.get pileIndex piles of
        Nothing ->
            piles

        Just pile ->
            let
                pile1 = Array.push card pile
            in
                Array.set pileIndex pile1 piles
