module OpenEngiadina.Status exposing (Status, decoder)

import Iso8601
import Json.Decode as JD
import Time


type alias Status =
    { id : String
    , date : Time.Posix
    , status : String
    , target : String
    }


decoder : JD.Decoder Status
decoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                if type_ == "Status" then
                    JD.map4 Status
                        (JD.field "id" JD.string)
                        (JD.field "date" Iso8601.decoder)
                        (JD.field "status" JD.string)
                        (JD.field "target" JD.string)

                else
                    JD.fail "Not a Status object."
            )
