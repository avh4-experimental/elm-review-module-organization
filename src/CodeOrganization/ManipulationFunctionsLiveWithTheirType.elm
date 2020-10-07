module CodeOrganization.ManipulationFunctionsLiveWithTheirType exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Review.Rule as Rule exposing (Rule)


{-| Reports violations of the following rule:

When a function uses some concrete type both as an input and an output,
both it and that type should be defined in the same module.

    config =
        [ CodeOrganization.ManipulationFunctionsLiveWithTheirType.rule
        ]


## Fail

    module Model exposing (Model)

    type alias Model = { ... }


    module Update exposing (update)
    import Model

    update : Msg -> Model -> (Model Cmd Msg)"
    update _ model = model


## Success

    module Main exposing (main)

    type alias Model = { ... }

    update : Msg -> Model -> (Model Cmd Msg)"
    update _ model = model


## When (not) to enable this rule

This rule is currently experimental.
It remains to be determined whether this is a good rule to generally enforce, or whether it is too strict.

Please report your experience at <https://github.com/avh4-experimental/elm-review-module-organization/issues>


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template avh4-experimental/elm-review-module-organization/example --rules CodeOrganization.ManipulationFunctionsLiveWithTheirType
```


## Notes

**Manipulation functions live with their type**: any functions that use some concrete type both as an input and an output should be defined in the same module the type itself is defined in (forces Model and update to be together)

    update : x -> Model -> Model
    update : x -> Model -> ( Model, c )
    update : x -> ( Model, config ) -> { newModel : Model, effect : e, event : Maybe ev }

    f : Result Model g -> Maybe ( Model, String )

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "CodeOrganization.ManipulationFunctionsLiveWithTheirType" ()
        |> Rule.withDeclarationEnterVisitor checkUpdateLikeFunctions
        |> Rule.fromModuleRuleSchema


checkUpdateLikeFunctions :
    Node Declaration
    -> moduleContext
    -> ( List (Rule.Error {}), moduleContext )
checkUpdateLikeFunctions node context =
    let
        checkError : Node Signature -> Maybe (Rule.Error {})
        checkError signature =
            if True then
                Just <|
                    Rule.error
                        { message = "Update.update and Model.Model should be defined in the same module"
                        , details =
                            [ "Update.update takes Model.Model as an input and returns it."
                            ]
                        }
                        (Node.range signature)

            else
                Nothing

        done errors =
            ( errors
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , context
            )
    in
    done <|
        case Node.value node of
            Declaration.FunctionDeclaration function ->
                case function.signature of
                    Nothing ->
                        Nothing

                    Just signature ->
                        case Node.value (Node.value signature).typeAnnotation of
                            TypeAnnotation.FunctionTypeAnnotation arg return ->
                                checkError signature

                            _ ->
                                Nothing

            _ ->
                Nothing
