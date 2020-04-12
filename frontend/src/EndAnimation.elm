module EndAnimation exposing (..)

import Array as Array exposing (Array)
import Card exposing (Card, getCardFromNumber)
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
        cards : Array Card
        , cardIndex : Int
        , offset : Int
    }


init : Model
init  =
     {
        cards = Array.empty
        , cardIndex = 0
        , offset = 0
     }


--  ####
--  ####      Update
--  ####


start :( Model, Cmd Msg )
start =
    (
        init
        , animate
    )


animate : Cmd Msg
animate =
    Process.sleep 80
    --Process.sleep 0
        |> Task.perform
            ( always DoAnimate )


type Msg
    = DoAnimate


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    let
        { cards, cardIndex, offset }  = model
        cardIndexOffset = cardIndex - offset
        maybeCard = if cardIndexOffset < 0 then getCardFromNumber ( 52 + cardIndexOffset ) else getCardFromNumber cardIndexOffset
    in
        case ( offset, maybeCard ) of
            ( 0, Just card ) ->
                (
                    { model
                    | cards = Array.push card model.cards
                    , cardIndex = cardIndex + 1
                    }
                    , animate
                )

            ( _, Just card ) ->
                (
                    { model
                    | cards = Array.set cardIndex card model.cards
                    , cardIndex = cardIndex + 1
                    }
                    , animate
                )

            ( _, Nothing ) ->
                (
                    { model
                    | cardIndex = 0
                    , offset = model.offset + 1
                    }
                    , animate
                )



--  ####
--  ####      View
--  ####


view : Model -> Html msg
view model =
    div [ class "end-animation-container" ]
        [ viewGrid model ]


viewGrid : Model -> Html msg
viewGrid { cards, offset } =
   div [ class "end-animation-grid" ]
        ( Array.indexedMap viewEndAnimationCard cards |> Array.toList )


viewEndAnimationCard : Int -> Card -> Html msg
viewEndAnimationCard index card =
    div [ class ( "end-animation-card" ++ ( String.fromInt ( modBy 3 index ))) ]
        [ Card.animate card ]

