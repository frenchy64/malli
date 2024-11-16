# Constraints

Typical nested schemas like `[:vector :string]` have corresponding
levels in the values they describe: the `:vector` for the outer
value and `:string for the first level of nesting.

The exceptions are composite schemas like `[:and :int [:< 42]]` which
describe the same value. This is problematic for generation: one
schema must generate values and the other filter them, yielding brittle
generators. Validators may also perform redundant checks, such has both
`:int` and `[:< 42]` needing to check the class of the validated value.
