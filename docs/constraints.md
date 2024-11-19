# Constraints

Typical nested schemas like `[:vector :string]` have corresponding
levels in the values they describe: the `:vector` for the outer
value and `:string for the first level of nesting.

The exceptions are composite schemas like `[:and :int [:< 42]]` which
describe the same value. This is problematic for generation: one
schema must generate values and the other filter them, yielding brittle
generators. Validators may also perform redundant checks, such has both
`:int` and `[:< 42]` needing to check the class of the validated value.

Constraints are intended to address this situation. A parent schema
has other "constraint" schemas attached to them which may collaborate
with the each other to yield reliable generators and lean validators.

For example, `[:int {:max 41}]` is actually two schemas (when constraints are enabled):
- the parent schema `:int`
- a constraint `[:max 41]`

## Reading this document

This document assumes this has been evaluated in the current namespace:

```clojure
(require '[malli.core :as m]
         '[malli.constraint.protocols :as mcp]
         '[malli.constraint :as mc])

(defn constraint-options []
  (-> {:registry (m/default-schemas)}
      mc/with-base-constraints))
```

## Activating Constraints

Constraints are an opt-in Malli feature. 

To activate constraints locally, use `mc/with-base-constraints` to upgrade
your options map. You can see an example in `constraint-options` above.

To activate the base constraints globally, call `(malli.constraint/activate-base-constraints!)`.

Constraints are themselves Schemas that also live in the registry.

Behind the scenes, an atom `malli.constraint.extension/constraint-extensions`
is used to configure constraints. The entire atom is ignored if `::m/constraint-options`
WIPWIPWIP

TODO rename constraint-options? or the atom? default-constraint-options?


## Constraint vs Schema

A constraint implements the same protocols as a Schema, and additionally
`malli.constraint.protocols/Constraint`.

Schemas that support schemas also implement
`malli.constraint.protocols/ConstrainedSchema`.

Validators on schemas ensure they check preconditions

## Constraint Extensions Registry

Constraints  schema represents

Constraint extensions are described as a schema in `malli.dev.constraint/ConstraintExtension`.
