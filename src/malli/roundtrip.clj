;; AI GENERATED CONTENT
;; Prompt:
;; In Clojure's malli library, there's the concept of roundtripping
;; m/parse and m/unparse operations. Some schemas simply do not
;; roundtrip. :orn has been specifically designed to reify its
;; selected branch into a Tag value that is looked up in its m/unparse
;; operation. But :or does not do this. The branch chosen is
;; completely transparent to the user. For example
;;
;; (let [schema [:or [:fn record?] [:orn [:i :int]]]]
;;   (->> 123
;;        (m/parse schema)
;;        (m/unparse schema)))
;; ;; => #malli.core.Tag{:key :i, :value 123}
;;
;; should return 123, but because the Tag is a record?, it was
;; unparsed by the first value. This situation is an example of a
;; non-roundtrippable schema. A roundtrippable schema is one who under
;; all situations compose m/parse + m/parse as the identity function,
;; up to pointer identity. But there can't be structural changes.
;; Please write a function that takes a schema and returns whether it
;; is roundtrippable. Add cases for each schema in the base malli
;; library. The reason the :or example is not roundtrippable is
;; because the the parsed output of the second child overlaps with the
;; accepted values of the first child. Use this kind of inductive
;; thinking to consider each schema individually, backed by an
;; extensible multimethod like many of the core malli algorithms, and
;; expose a public API that takes a schema and returns nil if the
;; schema is roundtrippable, otherwise a vector of maps where each map
;; explains the reason why this "path" into the schema (also available
;; as data in the map) is not roundtrippable. Then create a function
;; that parses this output and returns a human readable printout of
;; the results, which include actions to remedy the problem (such as
;; using :orn instead of :or). Use a single clj namespace and assume
;; malli is installed.

(ns malli.roundtrip
  (:require [malli.core :as m]
            [malli.util :as mu]
            [clojure.set :as set]))

(def base-types
  #{:int :string :keyword :boolean :double :uuid :fn :map :vector :set
    :cat :tuple :and :or :orn :maybe :sequential :record :any :nil
    :enum :repeat :multi :merge})

(defmulti parsed-overlap?
  (fn [a b] [(m/type a) (m/type b)]))

(defmethod parsed-overlap? :default [a b]
  false)

(defmethod parsed-overlap? [:int :string] [_ _] false)
(defmethod parsed-overlap? [:int :int] [_ _] true)
(defmethod parsed-overlap? [:string :string] [_ _] true)
(defmethod parsed-overlap? [:keyword :string] [_ _] false)
(defmethod parsed-overlap? [:keyword :keyword] [_ _] true)
;; :fn schemas: for soundness, we must be conservative
;; We need to inspect the :fn's predicate to determine overlap
;; If the :fn uses a predicate that could accept the same values as another schema,
;; they overlap
(defmethod parsed-overlap? [:fn :int] [a b]
  ;; Check if the :fn's predicate could accept integers
  (let [fn-pred (first (m/children a))]
    (cond
      ;; Check if it's a numeric predicate by comparing to known predicates
      (#{int? number? pos-int? neg-int? nat-int? pos? neg? zero?} fn-pred) true
      ;; If it's a clearly non-numeric predicate, no overlap
      (#{string? keyword? symbol? boolean? nil? map? vector? set? seq?} fn-pred) false
      ;; Otherwise, conservatively assume overlap
      :else true)))

(defmethod parsed-overlap? [:int :fn] [a b]
  (parsed-overlap? b a))

(defmethod parsed-overlap? [:fn :string] [a b]
  (let [fn-pred (first (m/children a))]
    (cond
      ;; If it's string?, it overlaps with :string
      (= string? fn-pred) true
      ;; If it's a clearly non-string predicate, no overlap
      (#{int? number? keyword? symbol? boolean? nil? map? vector? set? seq?
         pos-int? neg-int? nat-int? pos? neg? zero?} fn-pred) false
      ;; Otherwise, conservatively assume overlap
      :else true)))

(defmethod parsed-overlap? [:string :fn] [a b]
  (parsed-overlap? b a))

(defmethod parsed-overlap? [:fn :fn] [a b]
  ;; :fn schemas can overlap if they use the same or related predicates
  (let [fn-pred-a (first (m/children a))
        fn-pred-b (first (m/children b))]
    (cond
      ;; Same predicate definitely overlaps
      (= fn-pred-a fn-pred-b) true
      ;; int? and number? overlap
      (and (= fn-pred-a int?) (= fn-pred-b number?)) true
      (and (= fn-pred-a number?) (= fn-pred-b int?)) true
      ;; Clearly disjoint predicates don't overlap
      (and (#{int? number? pos-int? neg-int? nat-int?} fn-pred-a)
           (#{string? keyword? symbol? boolean? nil?} fn-pred-b)) false
      (and (#{string? keyword? symbol? boolean? nil?} fn-pred-a)
           (#{int? number? pos-int? neg-int? nat-int?} fn-pred-b)) false
      ;; Otherwise, conservatively assume overlap
      :else true)))
(defmethod parsed-overlap? [:orn :or] [a b] false)
(defmethod parsed-overlap? [:or :orn] [a b] false)
(defmethod parsed-overlap? [:orn :fn] [a b]
  ;; :orn produces Tag records
  ;; If the :fn uses record?, they overlap
  (let [fn-pred (first (m/children b))]
    (= record? fn-pred)))
(defmethod parsed-overlap? [:fn :orn] [a b]
  (parsed-overlap? b a))
(defmethod parsed-overlap? [:map :map] [a b] true)
(defmethod parsed-overlap? [:vector :set] [a b] false)
(defmethod parsed-overlap? [:set :set] [a b] true)
(defmethod parsed-overlap? [:any :any] [a b] true)
(defmethod parsed-overlap? [:nil :nil] [a b] true)
;; Predicate overlaps (using symbols as that's how malli represents them)
(defmethod parsed-overlap? ['number? :int] [_ _] true)
(defmethod parsed-overlap? [:int 'number?] [_ _] true)
(defmethod parsed-overlap? ['number? 'number?] [_ _] true)
(defmethod parsed-overlap? ['int? :int] [_ _] true)
(defmethod parsed-overlap? [:int 'int?] [_ _] true)
(defmethod parsed-overlap? ['int? 'int?] [_ _] true)
(defmethod parsed-overlap? ['int? 'number?] [_ _] true)
(defmethod parsed-overlap? ['number? 'int?] [_ _] true)
;; :fn with predicates - for soundness, assume they might overlap
(defmethod parsed-overlap? [:fn 'int?] [_ _] true)
(defmethod parsed-overlap? ['int? :fn] [_ _] true)
(defmethod parsed-overlap? [:fn 'number?] [_ _] true)
(defmethod parsed-overlap? ['number? :fn] [_ _] true)

(defmulti roundtrippable?
  (fn [schema] (m/type schema)))

(defn check-child-roundtrip
  [schema path-key]
  (let [children (m/children schema)
        child (first children)
        problems (vec (map (fn [p] (update p :path (fn [path] (vec (cons path-key path)))))
                           (roundtrippable? child)))]
    (not-empty problems)))

(defn check-multi-children-roundtrip
  [children]
  (let [problems (vec
                  (mapcat (fn [[i child]]
                            (map (fn [p] (update p :path (fn [path] (vec (cons i path)))))
                                 (roundtrippable? child)))
                          (map-indexed vector children)))]
    (not-empty problems)))

(defn explain
  [path msg & {:as opts}]
  (assoc opts :path path :problem msg))

(doseq [t [:int :string :keyword :boolean :double :uuid :nil :any :enum]]
  (defmethod roundtrippable? t [_] nil))

;; Predicates are also roundtrippable
(doseq [t ['int? 'string? 'keyword? 'boolean? 'number? 'uuid? 'nil? 'any? 'double? 'float?
           'pos-int? 'neg-int? 'nat-int? 'pos? 'neg? 'zero? 'some? 'fn? 'ifn?]]
  (defmethod roundtrippable? t [_] nil))

(defmethod roundtrippable? :fn [_] nil)

(defmethod roundtrippable? :map [schema]
  (let [children (m/children schema)
        problems (vec
                  (mapcat (fn [[k _props child]]
                            (map (fn [p] (update p :path (fn [path] (vec (cons k path)))))
                                 (roundtrippable? child)))
                          children))]
    (not-empty problems)))

(defmethod roundtrippable? :vector [schema]
  (check-child-roundtrip schema :vector))

(defmethod roundtrippable? :set [schema]
  (check-child-roundtrip schema :set))

(defmethod roundtrippable? :sequential [schema]
  (check-child-roundtrip schema :sequential))

(defmethod roundtrippable? :maybe [schema]
  (check-child-roundtrip schema :maybe))

(defmethod roundtrippable? :repeat [schema]
  (check-child-roundtrip schema :repeat))

(defmethod roundtrippable? :cat [schema]
  (check-multi-children-roundtrip (m/children schema)))

(defmethod roundtrippable? :tuple [schema]
  (check-multi-children-roundtrip (m/children schema)))

(defmethod roundtrippable? :and [schema]
  (check-multi-children-roundtrip (m/children schema)))

(defmethod roundtrippable? :merge [schema]
  (check-multi-children-roundtrip (m/children schema)))

(defmethod roundtrippable? :multi [schema]
  (check-multi-children-roundtrip (m/children schema)))

(defmethod roundtrippable? :or [schema]
  (let [branches (m/children schema)
        ;; First check if branches themselves are roundtrippable
        branch-problems (check-multi-children-roundtrip branches)
        ;; Then check for overlapping branches
        pairs (for [[i a] (map-indexed vector branches)
                    [j b] (map-indexed vector branches)
                    :when (< i j)]
                [i a j b])
        overlapping
        (for [[i a j b] pairs
              :when (parsed-overlap? a b)]
          (explain [i j]
            (str ":or branches at positions " i " and " j
                 " overlap in their parsed domain, so this schema "
                 "is not roundtrippable. If you need roundtripping,"
                 " use :orn instead of :or.")
            :schema schema
            :branch-a a
            :branch-b b))
        all-problems (vec (concat branch-problems (remove nil? overlapping)))]
    (not-empty all-problems)))

(defmethod roundtrippable? :orn [_] nil)

(defn explain-roundtrip
  "Takes a schema, returns nil if roundtrippable, else a vector of
   maps explaining the failure."
  [schema]
  (roundtrippable? schema))

(defn print-roundtrip-explanation
  "Takes a schema, prints a readable explanation if not roundtrippable,
   with suggested actions."
  [schema]
  (let [problems (explain-roundtrip schema)]
    (if (nil? problems)
      (println
        "Schema is roundtrippable: m/parse + m/unparse is the identity.")
      (doseq [{:keys [path problem branch-a branch-b]} problems]
        (println "Non-roundtrippable schema path:" path)
        (println "  Problem:" problem)
        (when branch-a
          (println "  Overlapping branch a:" branch-a))
        (when branch-b
          (println "  Overlapping branch b:" branch-b))
        (println "  Remedy: Use :orn instead of :or if you need"
                 " roundtrippable behavior.\n")))))
