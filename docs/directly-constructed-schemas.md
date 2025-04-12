# Directly constructed schemas

Schemas can be compiled into code that directly calls the relevant
constructors. This circumvents the need for both the map and vector
syntax.

The gotcha is that schemas must be resolvable in JVM Clojure.
Platform differences are ok, but should be hidden behind var schemas
that are themselves directly constructed.

```clojure
(require '[malli.direct :as md])

(md/direct :int)
;=> :int

(def platform-differences (md/direct [:int {:platform #?(:cljs :cljs :default :clj)}]))

(def permits-platform-differences (md/direct [:vector #'platform-differences]))
```
