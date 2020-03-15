port module CardsDragDrop exposing (..)

import Html5.DragDrop as DragDrop
import Json.Decode


--  ####
--  ####      MODEL
--  ####


type alias Model =
    {
        dragDrop : DragDrop.Model From To
    }

type From =
    Pile ( Int, Int )
    | Space Int


type To =
    Pile Int
    | Space Int


init : Model
init =
     {
        dragDrop = DragDrop.init
     }


-- ####
-- ####    UPDATE
-- ####


port dragstart : Json.Decode.Value -> Cmd msg


type Msg
    = DragDropMsg (DragDrop.Msg From To)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop

                model12 = Debug.log "model" model_
                result12 = Debug.log "result" result
            in
                (
                    { model
                    | dragDrop = model_
                    , piles =
                        case result of
                            Nothing ->
                                model.piles

                            Just ( ( pileIndexFrom, cardIndex ) , pileIndexTo , _ ) ->
                                if pileIndexFrom == pileIndexTo then
                                    model.piles
                                else
                                    moveCard pileIndexFrom cardIndex pileIndexTo model.piles

                    }
                    , DragDrop.getDragstartEvent msg_
                        |> Maybe.map (.event >> dragstart)
                        |> Maybe.withDefault Cmd.none
                )

