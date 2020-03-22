module Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Card = { suit : Suit, rank : Rank }


type Rank = Ace | King | Queen | Jack | N10 | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2


type Suit = Spades | Hearts | Clubs | Diamonds

type Color = Black | Red

defaultCard : Card
defaultCard = { suit = Spades, rank = N2 }


allSuits : List Suit
allSuits = [ Spades, Hearts, Clubs, Diamonds ]


allRanks : List Rank
allRanks = [ Ace, King, Queen, Jack, N10, N9, N8, N7, N6, N5, N4, N3, N2 ]
--allRanks = [ N4, N3, N2, Ace]

allCards : List Card
allCards =
    List.foldl addCardsofSuit [] allSuits


addCardsofSuit : Suit -> List Card -> List Card
addCardsofSuit suit cards =
    List.map (thisCard suit) allRanks
    |> List.append cards


thisCard : Suit -> Rank -> Card
thisCard suit rank = { suit = suit , rank = rank }


getSuit : Card -> Suit
getSuit { suit } = suit


getRank : Card -> Int
getRank { rank } =
    case rank of
        Ace ->
            0
        N2 ->
            1
        N3 ->
            2
        N4 ->
            3
        N5 ->
            4
        N6 ->
            5
        N7 ->
            6
        N8 ->
            7
        N9 ->
            8
        N10 ->
            9
        Jack ->
            10
        Queen ->
            11
        King ->
            12

getChar : Card -> Char
getChar { suit, rank } =
    ( case suit of
        Hearts ->
            65
        Diamonds ->
            78
        Clubs ->
            97
        Spades ->
            110
    )
    +
    ( case rank of
        Ace ->
            0
        N2 ->
            1
        N3 ->
            2
        N4 ->
            3
        N5 ->
            4
        N6 ->
            5
        N7 ->
            6
        N8 ->
            7
        N9 ->
            8
        N10 ->
            9
        Jack ->
            10
        Queen ->
            11
        King ->
            12
    )
    |> Char.fromCode


getColor : Card -> Color
getColor { suit } =
    case suit of
        Hearts ->
            Red
        Diamonds ->
            Red
        Clubs ->
            Black
        Spades ->
            Black


getColorClass : Color -> Attribute msg
getColorClass color =
    case color of
        Black ->
            class "card-black"
        Red ->
            class "card-red"

view : Card -> Html msg
view card =
    div [ class "card ", getColor card |> getColorClass ]
        [ text ( getChar card |> String.fromChar ) ]


-- ####
-- ####    VIEW
-- ####

cardPlaceholder : Html msg
cardPlaceholder =
    div [ class "card card-placeholder" ] [ text "A" ]


-- ####
-- ####    HELPER
-- ####


cardsSuccessivePile : Maybe Card -> Card -> Bool
cardsSuccessivePile maybeCard nextCard =
    case maybeCard of
        Nothing ->
            True
        Just card ->
            getColor card /= getColor nextCard
            &&
            getRank card == getRank nextCard + 1


cardsSuccessiveHome : Maybe Card -> Card -> Bool
cardsSuccessiveHome maybeCard nextCard =
    case maybeCard of
        Nothing ->
            getRank nextCard == 0
        Just card ->
            getSuit card == getSuit nextCard
            &&
            getRank card + 1 == getRank nextCard


