module CodeOrganization.ManipulationFunctionsLiveWithTheirTypeTest exposing (all)

import CodeOrganization.ManipulationFunctionsLiveWithTheirType exposing (rule)
import Review.Test
import Test exposing (..)


all : Test
all =
    describe "CodeOrganization.ManipulationFunctionsLiveWithTheirType"
        [ test "reports a simple function that is not with its manipulated type" <|
            \() ->
                [ String.join "\n"
                    [ "module TheType exposing (TheType)"
                    , ""
                    , "type TheType = TheType"
                    ]
                , String.join "\n"
                    [ "module ShouldBeMergedWithTheType exposing (update)"
                    , "import TheType"
                    , ""
                    , "f : TheType -> TheType"
                    , "f _ = Debug.todo \"\""
                    ]
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "ShouldBeMergedWithTheType"
                          , [ Review.Test.error
                                { message = "ShouldBeMergedWithTheType.f and TheType.TheType should be defined in the same module"
                                , details =
                                    [ "ShouldBeMergedWithTheType.f takes TheType.TheType as an input and returns it."

                                    -- TODO: explain why
                                    ]
                                , under = "f : TheType -> TheType"
                                }
                            ]
                          )
                        ]

        --test "reports a standard update function that is not with its Model" <|
        --    \() ->
        --        [ String.join "\n"
        --            [ "module Model exposing (Model)"
        --            , ""
        --            , "type alias Model = Never"
        --            ]
        --        , String.join "\n"
        --            [ "module Update exposing (update)"
        --            , "import Model"
        --            , ""
        --            , "update : Msg -> Model -> (Model, Cmd Msg)"
        --            , "update _ model = model"
        --            ]
        --        ]
        --            |> Review.Test.runOnModules rule
        --            |> Review.Test.expectErrorsForModules
        --                [ ( "Update"
        --                  , [ Review.Test.error
        --                        { message = "Update.update and Model.Model should be defined in the same module"
        --                        , details =
        --                            [ "Update.update takes Model.Model as an input and returns it."
        --
        --                            -- TODO: explain why
        --                            ]
        --                        , under = "update : Msg -> Model -> (Model, Cmd Msg)"
        --                        }
        --                    ]
        --                  )
        --                ]
        , test "does not report declarations that are not functions" <|
            \() ->
                String.join "\n"
                    [ "module M exposing (..)"
                    , "v : ()"
                    , "v = ()"
                    ]
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "does not report function declarations that do not have a type that is both input and output" <|
            \() ->
                String.join "\n"
                    [ "module M exposing (..)"
                    , "f : String -> Int"
                    , "f = Debug.todo \"\""
                    ]
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , todo "error message should refer to correct types"
        , todo "functions with multiple arguments"
        ]
