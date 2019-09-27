module GetStatus exposing (Model, Msg(..), init, main, subscriptions, update, view)

--

import Browser as B
import Html as H
import Http
import Iso8601
import Json.Decode as JD
import OpenEngiadina exposing (Activity)
import OpenEngiadina.Status as Status exposing (Status)
import Return exposing (Return)


main : Program {} Model Msg
main =
    B.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    List (Activity Status)


init : {} -> Return Msg Model
init flags =
    []
        |> Return.singleton
        |> Return.command (OpenEngiadina.getPublic Status.decoder ReceiveStatus)



-- UPDATE


type Msg
    = ReceiveStatus (Result Http.Error (List (Activity Status)))


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ReceiveStatus (Ok activities) ->
            activities
                |> Return.singleton

        _ ->
            model
                |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> B.Document Msg
view model =
    { title = "GetStatus"
    , body =
        [ H.h1 [] [ H.text "Status" ]
        , H.p [] [ "A listing of all Status objects created. The current status of an object (what is referenced in the target field) could be computed by taking the most recent status for the object." |> H.text ]
        , H.ul []
            (List.map
                (\activity ->
                    H.li []
                        [ H.dl []
                            [ H.dt [] [ "Actor who created the status" |> H.text ]
                            , H.dd [] [ activity.actor |> H.text ]
                            , H.dt [] [ "ID of the Status object" |> H.text ]
                            , H.dd [] [ activity.object.id |> H.text ]
                            , H.dt [] [ "Status" |> H.text ]
                            , H.dd [] [ activity.object.status |> H.text ]
                            , H.dt [] [ "Target (id of the object status concerns)" |> H.text ]
                            , H.dd [] [ activity.object.target |> H.text ]
                            , H.dt [] [ "Date" |> H.text ]
                            , H.dd [] [ activity.object.date |> Iso8601.fromTime |> H.text ]
                            ]
                        ]
                )
                model
            )
        ]
    }
