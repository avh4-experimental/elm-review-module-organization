module CodeOrganization.ManipulationFunctionsLiveWithTheirTypeTest exposing (all)

import CodeOrganization.ManipulationFunctionsLiveWithTheirType exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "CodeOrganization.ManipulationFunctionsLiveWithTheirType"
        [ test "reports a standard update function that is not with its Model" <|
            \() ->
                [ String.join "\n"
                    [ "module Model exposing (Model)"
                    , ""
                    , "type alias Model = Never"
                    ]
                , String.join "\n"
                    [ "module Update exposing (update)"
                    , "import Model"
                    , ""
                    , "update : Msg -> Model -> (Model, Cmd Msg)"
                    , "update _ model = model"
                    ]
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Update"
                          , [ Review.Test.error
                                { message = "Update.update and Model.Model should be defined in the same module"
                                , details =
                                    [ "Update.update takes Model.Model as an input and returns it."

                                    -- TODO: explain why
                                    ]
                                , under = "update : Msg -> Model -> (Model, Cmd Msg)"
                                }
                            ]
                          )
                        ]
        , test "does not report declarations that are not functions" <|
            \() ->
                String.join "\n"
                    [ "module M exposing (..)"
                    , "v : ()"
                    , "v = ()"
                    ]
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
