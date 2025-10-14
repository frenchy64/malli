# Malli Subtyping Algorithm

This directory contains an implementation of a subtyping algorithm for Malli schemas.

## Overview

The subtyping algorithm determines whether one schema `S` is a subtype of another schema `T`. Formally, `S` is a subtype of `T` (written `S <: T`) if every value that validates against `S` also validates against `T`. In other words, `(m/validator S)` admits a subset of values that `(m/validator T)` does.

## API

### `subtype?`

```clojure
(subtype? S T)
(subtype? S T opts)
```

Returns `true` if schema `S` is a subtype of schema `T`, `false` otherwise.

**Arguments:**
- `S` - The potential subtype schema
- `T` - The potential supertype schema  
- `opts` - (optional) Options map (same as `m/schema`)

**Examples:**

```clojure
(require '[malli.subtype :as ms])

(ms/subtype? :int :any)                ;=> true
(ms/subtype? [:int {:min 5}] :int)     ;=> true
(ms/subtype? [:enum 1 2] [:enum 1 2 3]) ;=> true
(ms/subtype? [:map [:x :int] [:y :string]]
             [:map [:x :int]])         ;=> true
```

## Architecture

The implementation uses three multimethods to provide an extensible subtyping algorithm:

### 1. `subtype-same?*` - Same Type Multimethod

Dispatches on schema type when both schemas have the same type.

```clojure
(m/-type S) = (m/-type T) => subtype-same?* type S T opts
```

**Example:** Checking if `[:int {:min 5}]` is a subtype of `[:int {:min 3}]`

### 2. `subtype-left?*` - Left Dispatch Multimethod

Dispatches on the left (subtype candidate) schema type when types differ.

```clojure
(m/-type S) ≠ (m/-type T) => subtype-left?* (m/-type S) S T opts
```

**Example:** Checking if `[:and :int [:> 0]]` is a subtype of `:int`

### 3. `subtype-right?*` - Right Dispatch Multimethod

Dispatches on both schema types for concrete type checking. Called from `subtype-left?*` implementations.

```clojure
subtype-right?* (m/-type S) (m/-type T) S T opts
```

**Example:** Checking if `:int` is a subtype of `[:or :int :string]`

## Type Relationships

### Top Types

- `:any` - The top type; all types are subtypes of `:any`
- `:some` - All non-nil types are subtypes of `:some`

### Simple Types

Simple types are reflexive (each is a subtype of itself) and can have property constraints:

```clojure
:nil :string :int :float :double :boolean :keyword :symbol
:qualified-keyword :qualified-symbol :uuid
```

**Note:** Malli types are distinct - `:int` is NOT a subtype of `:float` because integer values don't validate against `:float`.

### Properties

Schemas with `:min` and `:max` properties are subtypes when more restrictive:

```clojure
(ms/subtype? [:int {:min 5 :max 10}]
             [:int {:min 0 :max 20}])  ;=> true
```

### Union Types (`:or`)

`S <: [:or T1 T2 ...]` if `S` is a subtype of some `Ti`.

```clojure
(ms/subtype? :int [:or :int :string])  ;=> true
```

### Intersection Types (`:and`)

`[:and S1 S2 ...] <: T` if there exists `Si` of the same base type as `T` and `Si <: T`, or if all `Si <: T`.

```clojure
(ms/subtype? [:and :int [:> 0]] :int)  ;=> true
```

### Enums

`[:enum v1 v2] <: [:enum w1 w2 w3]` if `{v1 v2} ⊆ {w1 w2 w3}`.

```clojure
(ms/subtype? [:enum 1 2] [:enum 1 2 3]) ;=> true
```

### Collections

For collections like `:vector`, `:sequential`, `:set`:
- Element types must be in subtype relation
- Size constraints (`:min`, `:max`) must be more restrictive

```clojure
(ms/subtype? [:vector [:int {:min 5}]]
             [:vector :int])            ;=> true
```

### Tuples

Tuples are subtypes when:
- Same length
- Corresponding elements are in subtype relation
- Tuples are subtypes of `:vector` and `:sequential` with compatible element types

```clojure
(ms/subtype? [:tuple :int :string]
             [:tuple :int :string])     ;=> true
(ms/subtype? [:tuple :int :string]
             [:vector :any])            ;=> true
```

### Maps

`S <: T` when for each required entry `[k Tk]` in `T`, `S` has an entry `[k Sk]` where `Sk <: Tk`.

```clojure
(ms/subtype? [:map [:x :int] [:y :string]]
             [:map [:x :int]])          ;=> true
```

### Map-of

`[:map-of Sk Sv] <: [:map-of Tk Tv]` if `Sk <: Tk` and `Sv <: Tv`.

```clojure
(ms/subtype? [:map-of :qualified-keyword :int]
             [:map-of :keyword :int])   ;=> true
```

### Negation (`:not`)

Negation is contravariant: `[:not S] <: [:not T]` when `T <: S`.

```clojure
(ms/subtype? [:not :int] [:not [:int {:min 5}]]) ;=> true
```

### Maybe (`:maybe`)

`S <: [:maybe T]` if `S` is `:nil` or `S <: T`.

```clojure
(ms/subtype? :nil [:maybe :int])       ;=> true
```

## Limitations

### Comparator Schemas

The current implementation does not deeply analyze comparator schemas like `[:>= 5]`, `[:<= 100]`, etc. This means:

```clojure
;; Not currently supported:
(ms/subtype? [:int {:min 5}] [:and :int [:>= 5]])  ;=> false

;; Not currently supported:
(ms/subtype? [:and :int [:> 0] [:<= 100]]
             [:and :int [:> 0]])                   ;=> false
```

To fully support these, the algorithm would need to understand and reason about the semantics of comparator predicates.

### Function Schemas

Function schemas (`:fn`) cannot be proven to be subtypes in the general case without function analysis:

```clojure
(ms/subtype? [:fn int?] [:fn number?]) ;=> false (conservative)
```

### Predicate Schemas

Similar to function schemas, arbitrary predicate schemas cannot be analyzed:

```clojure
(ms/subtype? int? number?) ;=> false (uses form equality)
```

## Testing

The implementation includes comprehensive tests:

- **Unit tests** - Test each type combination
- **Tabular tests** - Systematic testing of type relationships
- **Generative tests** - Property-based testing using `test.check`

Run tests with:

```bash
./bin/kaocha --focus malli.subtype-test
```

## Extending the Algorithm

To add support for a new schema type:

1. Implement `subtype-same?*` for when both schemas have the new type
2. Implement `subtype-left?*` for when the left schema has the new type (optional)
3. Implement `subtype-right?*` methods for relationships with other types

Example:

```clojure
(defmethod subtype-same?* :my-type [_ S T opts]
  ;; Implementation for when both S and T are :my-type
  ...)

(defmethod subtype-right?* [:my-type :any] [_ _ S T opts]
  ;; :my-type is always a subtype of :any
  true)
```

## References

- Subtyping in type systems: https://en.wikipedia.org/wiki/Subtyping
- Malli core: https://github.com/metosin/malli
