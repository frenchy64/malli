# Roundtrip Linter Implementation

## Overview

The roundtrip linter checks if Malli schemas are "roundtrippable" - that is, whether `m/parse` followed by `m/unparse` acts as the identity function.

## Key Concepts

### Roundtrippable Schemas

A schema is roundtrippable if for all valid inputs `x`:
```clojure
(= x (m/unparse schema (m/parse schema x)))
```

### Non-roundtrippable Schemas

The main cause of non-roundtrippability is when `:or` branches have overlapping parsed outputs. For example:

```clojure
[:or [:int] number?]
```

This is not roundtrippable because:
1. Both branches can parse integers
2. If an integer is parsed by the first branch, it's returned as-is
3. If parsed by the second branch (if first fails), it's still an integer
4. On unparse, there's ambiguity about which branch to use

### Why :orn is Roundtrippable

`:orn` (tagged or) solves this by wrapping parsed values in Tags:

```clojure
[:orn [:i [:int]] [:n number?]]
```

When parsing `123`, you get `#malli.core.Tag{:key :i, :value 123}` or `#malli.core.Tag{:key :n, :value 123}`, which contains enough information to unparse correctly.

## Implementation Details

### Overlap Detection

The `parsed-overlap?` multimethod determines if two schemas have overlapping parsed outputs:

- **Keyword schemas** (`:int`, `:string`, etc.): Overlap based on type compatibility
- **Predicate schemas** (`int?`, `number?`, etc.): Overlap if predicates accept the same values
- **:fn schemas**: Inspect the predicate function to determine overlap
  - `[:fn int?]` overlaps with `[:int]` and `number?`
  - `[:fn string?]` doesn't overlap with `[:int]`
  - For soundness, unknown predicates are assumed to potentially overlap
- **:orn schemas**: Tag outputs are records, so `[:fn record?]` overlaps with `:orn`
- **Container schemas**: `:map`, `:vector`, etc. overlap if they're the same type

### Recursive Checking

The `roundtrippable?` multimethod recursively checks schemas:

- **Simple types** (`:int`, `:string`, predicates): Always roundtrippable
- **:fn schemas**: Always roundtrippable (pure functions assumed)
- **:orn schemas**: Always roundtrippable (by design)
- **Container schemas** (`:map`, `:vector`, `:set`, `:tuple`, etc.): Roundtrippable if children are
- **:or schemas**: Roundtrippable if:
  1. All branches are roundtrippable, AND
  2. No two branches have overlapping parsed outputs
- **:and, :maybe, :repeat**: Roundtrippable if children are

## API

### `explain-roundtrip`

```clojure
(rt/explain-roundtrip schema)
;; => nil if roundtrippable
;; => [{:path [...], :problem "...", :schema ..., ...}] if not roundtrippable
```

Returns `nil` if the schema is roundtrippable, otherwise a vector of maps explaining the problems.

### `print-roundtrip-explanation`

```clojure
(rt/print-roundtrip-explanation schema)
```

Prints a human-readable explanation with suggested fixes.

## Test Coverage

The test suite includes 23 tests with 84 assertions covering:

1. **Simple types**: All base types and predicates
2. **:or schemas**: Non-overlapping, overlapping, nested
3. **:orn schemas**: Various combinations
4. **Container schemas**: Maps, vectors, sets, tuples
5. **:fn schemas**: Different predicates, overlap detection
6. **Complex nested schemas**: Deeply nested structures
7. **Edge cases**: :any, :nil, :enum, multiple branches
8. **Original example**: `[:fn record?]` with `:orn`

## Soundness

The implementation errs on the side of soundness:

- Unknown `:fn` predicates are assumed to potentially overlap
- Conservative checks ensure no false negatives (claiming roundtrippability when it's not)
- May produce false positives (claiming non-roundtrippability when it might be)

## Example Usage

```clojure
(require '[malli.roundtrip :as rt])

;; Check a simple schema
(rt/explain-roundtrip [:or [:int] [:string]])
;; => nil (roundtrippable)

;; Check a problematic schema
(rt/explain-roundtrip [:or [:int] number?])
;; => [{:path [0 1],
;;      :problem ":or branches at positions 0 and 1 overlap...",
;;      :schema [:or [:int] number?],
;;      :branch-a [:int],
;;      :branch-b number?}]

;; Print explanation
(rt/print-roundtrip-explanation [:or [:int] number?])
;; Prints:
;; Non-roundtrippable schema path: [0 1]
;;   Problem: :or branches at positions 0 and 1 overlap in their parsed domain...
;;   Overlapping branch a: [:int]
;;   Overlapping branch b: number?
;;   Remedy: Use :orn instead of :or if you need roundtrippable behavior.
```
