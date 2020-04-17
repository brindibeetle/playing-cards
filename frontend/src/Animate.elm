module Animate exposing (..)

import Array as Array exposing (Array)
import Card exposing (Card)
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, style)

import Process
import Random as Random exposing (..)
import Task

--  ####
--  ####      Model
--  ####


numberOfIterations : Int
numberOfIterations = 8


type alias Model =
    {
        maybeCard : Maybe Card
        , from : Coordinates
        , to : Coordinates
        , current : Coordinates
        , iteration : Int
    }


init : Model
init =
     {
        maybeCard = Nothing
        , from = initCoordinates
        , to = initCoordinates
        , current = initCoordinates
        , iteration = 0
     }


animating : Model -> Bool
animating { maybeCard } =
    case maybeCard of
        Nothing ->
            False

        _ ->
            True

--  ####
--  ####      Coordinates
--  ####


type alias Coordinates =
    {
        x : Int
        , y : Int
    }


initCoordinates : Coordinates
initCoordinates = { x = 0, y = 0 }


distance : Coordinates -> Coordinates -> Coordinates
distance from to =
    { x = to.x - from.x, y = to.y - from.y }


move : Coordinates -> Coordinates -> Coordinates
move from offset =
    { x = from.x + offset.x, y = from.y + offset.y }


stepDistance : Int -> Int -> Coordinates -> Coordinates
stepDistance step steps offset  =
    { x = ( step * offset.x ) // steps , y = ( step * offset.y ) // steps }


--  ####
--  ####      View
--  ####

getStyles : Int -> Int -> List (Attribute msg)
getStyles x y =
    [ style "position" "absolute"
    , style "left" ( ( String.fromInt x ) ++ "vw" )
    , style "top" ( ( String.fromInt y ) ++ "vh" )
    ]

view : Model -> Html msg
view model  =
    let
        { maybeCard, from, to, current, iteration }  = model
    in
        case maybeCard of
            Nothing ->
                div [] []

            Just card ->
                div ( getStyles current.x current.y )
                    [ Card.view card ]


--  ####
--  ####      Update
--  ####


start : Card -> Coordinates -> Coordinates -> ( Model, Cmd Msg )
start card from to =
    (
        { maybeCard = Just card
        , from = from
        , to = to
        , current = from
        , iteration = 0
        }
        , fly 0 )


fly : Int -> Cmd Msg
fly index =
    Process.sleep 25
        |> Task.perform
            ( always DoFly )


type Msg
    = DoFly


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    let
        { maybeCard, from, to, current, iteration }  = model
    in
        case maybeCard of
            Nothing ->
                ( model, Cmd.none )

            Just card ->
                if iteration == numberOfIterations + 1 then
                    (
                        { model
                        | maybeCard = Nothing
                        }
                        , Cmd.none
                    )
                else
                    (
                        { model
                        | current = Debug.log "Animate current = " ( distance from to |> stepDistance iteration numberOfIterations |> move from )
                        , iteration = iteration + 1
                        }
                       , fly iteration
                )


