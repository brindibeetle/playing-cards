module Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Card = { suit : Suit, rank : Rank }


type Rank = Ace | King | Queen | Jack | N10 | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2


type Suit = Spades | Hearts | Clubs | Diamonds


defaultCard : Card
defaultCard = { suit = Spades, rank = N2 }


allSuits : List Suit
allSuits = [ Spades, Hearts, Clubs, Diamonds ]


allRanks : List Rank
allRanks = [ Ace, King, Queen, Jack, N10, N9, N8, N7, N6, N5, N4, N3, N2 ]


allCards : List Card
allCards =
    List.foldl addCardsofSuit [] allSuits


addCardsofSuit : Suit -> List Card -> List Card
addCardsofSuit suit cards =
    List.map (thisCard suit) allRanks
    |> List.append cards


thisCard : Suit -> Rank -> Card
thisCard suit rank = { suit = suit , rank = rank }


getImage : Card -> Attribute msg
getImage { suit, rank } =
    String.concat
        [ "src/resources/"
        , case suit of
            Spades ->
                "spades"
            Hearts ->
                "hearts"
            Clubs ->
                "clubs"
            Diamonds ->
                "diamonds"
        , " "
        , case rank of
            Ace ->
                "ace"
            King ->
                "king"
            Queen ->
                "queen"
            Jack ->
                "jack"
            N10 ->
                "10"
            N9 ->
                "9"
            N8 ->
                "8"
            N7 ->
                "7"
            N6 ->
                "6"
            N5 ->
                "5"
            N4 ->
                "4"
            N3 ->
                "3"
            N2 ->
                "2"
        , ".png"
        ]
    |> src


view : Card -> Html msg
view card =
    div [ class "card" ]
        [ img [ getImage card, class "card" ] [ ]
        ]

