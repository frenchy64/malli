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

The main cause of non-roundtrippability is when `:or` branches have overlapping parsed outputs **and at least one branch has a non-simple (transforming) parser**.

#### Simple vs. Non-simple Parsers

Malli schemas have a "simple-parser" property that indicates whether the parser transforms values:
- **Simple parser** (`-parser-info :simple-parser` is `true`): The parse/unparse operations are identity functions
- **Non-simple parser** (`-parser-info :simple-parser` is `nil` or `false`): The parser transforms values

Examples:
- Simple parsers: `:int`, `:string`, predicates (`int?`, `number?`), `:fn` schemas
- Non-simple parsers: `:orn` (wraps values in Tags)

#### When :or is NOT Roundtrippable

An `:or` schema is not roundtrippable when:
1. **At least one branch has a non-simple parser**, AND
2. **The branches have overlapping parsed outputs**

Example of non-roundtrippable schema:
```clojure
[:or [:fn record?] [:orn [:i [:int]]]]
```

This is not roundtrippable because:
1. The `:orn` branch has a non-simple parser (produces Tag records)
2. The `:fn record?` branch accepts records (overlaps with Tag)
3. When unparsing a Tag, the `:fn record?` branch matches and returns the Tag as-is
4. Result: `123` → `Tag{:i 123}` → `Tag{:i 123}` ≠ `123`

#### When :or IS Roundtrippable

An `:or` schema IS roundtrippable when **all branches have simple parsers**, even if types overlap:

```clojure
[:or [:int] number?]
```

This IS roundtrippable because:
1. Both `:int` and `number?` have simple parsers (identity parse/unparse)
2. Even though integers match both branches, no transformation occurs
3. Result: `123` → `123` → `123` = `123`

### Why :orn is Roundtrippable

`:orn` (tagged or) solves this by wrapping parsed values in Tags:

```clojure
[:orn [:i [:int]] [:n number?]]
```

When parsing `123`, you get `#malli.core.Tag{:key :i, :value 123}` or `#malli.core.Tag{:key :n, :value 123}`, which contains enough information to unparse correctly.

## Implementation Details

### Overlap Detection

The `parsed-overlap?` multimethod determines if two schemas have overlapping parsed outputs. However, overlap only matters when at least one schema has a non-simple parser.

Key overlap rules:
- **Keyword schemas** (`:int`, `:string`, etc.): Overlap based on type compatibility
- **Predicate schemas** (`int?`, `number?`, etc.): Overlap if predicates accept the same values
- **:fn schemas**: Have simple parsers, so overlap doesn't cause issues
- **:orn schemas**: Have non-simple parsers, overlap detection is critical
  - Two `:orn` schemas can overlap (both produce Tag records)
  - `:orn` overlaps with `[:fn record?]` (Tag IS a record)
- **Container schemas**: `:map`, `:vector`, etc. overlap if they're the same type

### Recursive Checking

The `roundtrippable?` multimethod recursively checks schemas:

- **Simple types** (`:int`, `:string`, predicates): Always roundtrippable
- **:fn schemas**: Always roundtrippable (simple parsers)
- **:orn schemas**: Always roundtrippable (by design, tags disambiguate)
- **Container schemas** (`:map`, `:vector`, `:set`, `:tuple`, etc.): Roundtrippable if children are
- **:or schemas**: Roundtrippable if:
  1. All branches are roundtrippable, AND
  2. Either:
     - All branches have simple parsers (no overlap issues), OR
     - No two non-simple branches have overlapping parsed outputs
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

The test suite includes 23 tests with 78 assertions covering:

1. **Simple types**: All base types and predicates
2. **:or schemas**: Simple parsers (roundtrippable even with overlap), non-simple branches
3. **:orn schemas**: Various combinations, overlap with `:fn record?`
4. **Container schemas**: Maps, vectors, sets, tuples
5. **:fn schemas**: Simple parser behavior
6. **Complex nested schemas**: Deeply nested structures
7. **Edge cases**: :any, :nil, :enum, multiple branches
8. **Original example**: `[:fn record?]` with `:orn`
9. **Simple parser understanding**: Tests demonstrating when overlap is OK

## Soundness

The implementation errs on the side of soundness by using the simple-parser API:

- Schemas with simple (non-transforming) parsers don't cause roundtrip issues, even with type overlap
- Only non-simple parsers (like `:orn`) require overlap checking
- This provides precise analysis: no false negatives or false positives for the simple parser case
- Conservative checks for complex overlaps ensure soundness

## Example Usage

```clojure
(require '[malli.roundtrip :as rt])

;; Check a schema with simple parsers - roundtrippable even with overlap
(rt/explain-roundtrip [:or [:int] number?])
;; => nil (roundtrippable! Both have simple parsers)

;; Check a schema with non-simple parser
(rt/explain-roundtrip [:or [:fn record?] [:orn [:i [:int]]]])
;; => [{:path [0 1],
;;      :problem ":or branches at positions 0 and 1 overlap...",
;;      :schema [:or [:fn record?] [:orn [:i [:int]]]],
;;      :branch-a [:fn record?],
;;      :branch-b [:orn [:i [:int]]]}]

;; Print explanation
(rt/print-roundtrip-explanation [:or [:fn record?] [:orn [:i [:int]]]])
;; Prints:
;; Non-roundtrippable schema path: [0 1]
;;   Problem: :or branches at positions 0 and 1 overlap in their parsed domain...
;;   Overlapping branch a: [:fn record?]
;;   Overlapping branch b: [:orn [:i [:int]]]
;;   Remedy: Use :orn instead of :or if you need roundtrippable behavior.
```
