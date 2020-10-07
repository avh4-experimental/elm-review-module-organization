# elm-review-module-organization

This package experiments with [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules
to guide good module organization in Elm projects.
It remains to be determined whether this is a good rule to generally enforce, or whether it is too strict.

Please report your experience at <https://github.com/avh4-experimental/elm-review-module-organization/issues>


## Provided rules

- [`CodeOrganization.ManipulationFunctionsLiveWithTheirType`](https://package.elm-lang.org/packages/avh4-experimental/elm-review-module-organization/1.0.0/CodeOrganization-ManipulationFunctionsLiveWithTheirType) - Reports functions that should live with the type they manipulate.


## Configuration

```elm
module ReviewConfig exposing (config)

import CodeOrganization.ManipulationFunctionsLiveWithTheirType
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ CodeOrganization.ManipulationFunctionsLiveWithTheirType.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template avh4-experimental/elm-review-module-organization/preview
```
