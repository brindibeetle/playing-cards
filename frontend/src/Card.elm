module Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Card = { suit : Suit, rank : Rank }


type Rank = Ace | King | Queen | Jack | N10 | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2


type Suit = Spades | Hearts | Clubs | Diamonds

type Color = Black | Red | DarkBrown | LightBrown | Whitish | LightBlue | DarkBlue


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


getChars : Card -> List (Int, Color)
getChars { suit, rank } =
    let
        ( suitOffset, suitColor ) =
            ( case suit of
                Clubs ->
                    ( 33, Black )
                Diamonds ->
                    ( 57, Red )
                Hearts ->
                    ( 81, Red )
                Spades ->
                    ( 105, Black )
            )
    in
        ( 129, Whitish )
        ::
        ( case rank of
            Ace ->
                [ ( suitOffset + 0, suitColor ) ]
            N2 ->
                [ ( suitOffset + 1, suitColor ) ]
            N3 ->
                [ ( suitOffset + 2, suitColor ) ]
            N4 ->
                [ ( suitOffset + 3, suitColor ) ]
            N5 ->
                [ ( suitOffset + 4, suitColor ) ]
            N6 ->
                [ ( suitOffset + 5, suitColor ) ]
            N7 ->
                [ ( suitOffset + 6, suitColor ) ]
            N8 ->
                [ ( suitOffset + 7, suitColor ) ]
            N9 ->
                [ ( suitOffset + 8, suitColor ) ]
            N10 ->
                [ ( suitOffset + 9, suitColor ) ]
            Jack ->
                [ ( suitOffset + 10, Black )
                , ( suitOffset + 11, DarkBrown )
                , ( suitOffset + 12, LightBrown )
                , ( suitOffset + 13, Red )
                ]
            Queen ->
                [ ( suitOffset + 14, Black )
                , ( suitOffset + 15, DarkBrown )
                , ( suitOffset + 16, LightBrown )
                , ( suitOffset + 17, Red )
                ]
            King ->
                [ ( suitOffset + 18, Black )
                , ( suitOffset + 19, DarkBrown )
                , ( suitOffset + 20, LightBrown )
                , ( suitOffset + 21, Red )
                ]
        )


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
            class "color-black"

        Red ->
            class "color-red"

        DarkBrown ->
            class "color-darkbrown"

        LightBrown ->
            class "color-lightbrown"

        Whitish ->
            class "color-whitish"

        LightBlue ->
            class "color-lightblue"

        DarkBlue ->
            class "color-darkblue"



view : Card -> Html msg
view card =
    div [ class "char-holder" ]
        ( List.map viewChar ( getChars card ) )


viewChar : ( Int, Color ) -> Html msg
viewChar ( int, color ) =
    div [ class "char ", getColorClass color ]
    [ text ( String.fromChar (Char.fromCode int)) ]
    --[ text ( String.fromChar char ) ]

-- ####
-- ####    VIEW
-- ####


getCharsBack : List ( Int, Color )
getCharsBack =
    [ ( 129, Whitish )
    , ( 130, Black )
    , ( 133, DarkBlue )
    , ( 133, LightBlue )
    ]

viewBack : Html msg
viewBack =
    div [ class "char-holder" ]
        ( List.map viewChar ( getCharsBack ) )


cardPlaceholder : Html msg
cardPlaceholder =
    div [ class "card card-placeholder" ] [ viewBack ]


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


