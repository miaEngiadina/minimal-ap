module OpenEngiadina exposing (Activity, getPublic)

import Http
import Iso8601
import Json.Decode as JD
import Time


defaultUrl =
    "https://ap-dev.miaengiadina.ch/"


{-| An ActivityStreams Activity

See <https://www.w3.org/TR/activitystreams-core/#activities>.

Note that the object can be anything, thus Activity is a parameterized type.

-}
type alias Activity a =
    { type_ : String
    , id : String
    , actor : String
    , published : Time.Posix
    , object : a
    }


{-| Decoder for Activity.

You must supply the object decoder.

-}
activityDecoder : JD.Decoder object -> JD.Decoder (Activity object)
activityDecoder objectDecoder =
    JD.map5 Activity
        (JD.field "type" JD.string)
        (JD.field "id" JD.string)
        (JD.field "actor" JD.string)
        (JD.field "published" Iso8601.decoder)
        (JD.field "object" objectDecoder)


{-| Collection decoder
-}
collectionDecoder : JD.Decoder object -> JD.Decoder (List (Activity object))
collectionDecoder objectDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                if type_ == "Collection" then
                    -- Decode objects to Maybe
                    JD.field "items"
                        (activityDecoder objectDecoder
                            |> JD.maybe
                            |> JD.list
                        )

                else
                    JD.fail "Not a Collection."
            )
        -- Filter out activities that could not be decoded
        |> JD.map (List.filterMap identity)


{-| Get all public activities of a certain object type
-}
getPublic : JD.Decoder object -> (Result Http.Error (List (Activity object)) -> msg) -> Cmd msg
getPublic objectDecoder toMsg =
    Http.get
        { url = defaultUrl ++ "public"
        , expect =
            Http.expectJson toMsg
                (collectionDecoder objectDecoder)
        }
