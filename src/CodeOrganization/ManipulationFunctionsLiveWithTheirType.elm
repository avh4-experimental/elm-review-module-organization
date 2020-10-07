module CodeOrganization.ManipulationFunctionsLiveWithTheirType exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
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
        runRule :
            ( Node Signature
            , { inputType : TypeAnnotation
              , outputType : TypeAnnotation
              }
            )
            -> Maybe (Rule.Error {})
        runRule ( signature, { inputType, outputType } ) =
            if isEqualType inputType outputType then
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
    getFunctionDeclaration node
        |> Maybe.andThen getTypeAnnotation
        |> Maybe.andThen (mapSecondMaybe collectInputAndOutputTypes)
        |> Maybe.andThen runRule
        |> done


getFunctionDeclaration : Node Declaration -> Maybe Function
getFunctionDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            Just function

        _ ->
            Nothing


getTypeAnnotation : Function -> Maybe ( Node Signature, TypeAnnotation )
getTypeAnnotation function =
    case function.signature of
        Nothing ->
            Nothing

        Just signature ->
            Just
                ( signature
                , Node.value (Node.value signature).typeAnnotation
                )


collectInputAndOutputTypes :
    TypeAnnotation
    ->
        Maybe
            { inputType : TypeAnnotation
            , outputType : TypeAnnotation
            }
collectInputAndOutputTypes typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.FunctionTypeAnnotation arg return ->
            Just
                { inputType = Node.value arg
                , outputType = Node.value return
                }

        _ ->
            Nothing



-- Generic Elm AST functions


isEqualType : TypeAnnotation -> TypeAnnotation -> Bool
isEqualType a b =
    case ( a, b ) of
        ( GenericType a1, GenericType b1 ) ->
            a1 == b1

        --(Typed ()
        ( Unit, Unit ) ->
            True

        ( Tupled a1, Tupled b1 ) ->
            (List.length a1 == List.length b1)
                && (List.map2 Tuple.pair a1 b1
                        |> List.all
                            (\( at, bt ) ->
                                isEqualType
                                    (Node.value at)
                                    (Node.value bt)
                            )
                   )
       ()



--= GenericType String
--  | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
--  | Unit
--  | Tupled (List (Node TypeAnnotation))
--  | Record RecordDefinition
--  | GenericRecord (Node String) (Node RecordDefinition)
--  | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)
-- Generic Maybe functions


mapSecondMaybe : (a -> Maybe b) -> (( x, a ) -> Maybe ( x, b ))
mapSecondMaybe f ( x, a ) =
    f a
        |> Maybe.map (\b -> ( x, b ))
