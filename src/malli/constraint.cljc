(ns malli.constraint
  (:require [clojure.core :as cc]
            [clojure.set :as set]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr])
  #?(:clj (:import (clojure.lang IPersistentVector))))

(defn constraint
  ([?constraint] (constraint ?constraint nil))
  ([?constraint options]
   (cond
     (mcp/-constraint? ?constraint) ?constraint
     ;; reserving for now for "contains" constraints for :map. will be an extension per-schema.
     (keyword? ?constraint) (-fail! ::constraints-must-be-vectors {:outer-schema (-> options ::m/constraint-context :type)
                                                                   :constraint ?constraint})
     (vector? ?constraint) (let [v #?(:clj ^IPersistentVector ?constraint, :cljs ?constraint)
                                 n #?(:bb (count v) :clj (.count v), :cljs (count v))
                                 op #?(:clj (.nth v 0), :cljs (nth v 0))
                                 ?p (when (> n 1) #?(:clj (.nth v 1), :cljs (nth v 1)))
                                 prs (or (-> options ::m/constraint-context :parse-constraint)
                                         (-fail! ::missing-parse-constraint-options {:constraint ?constraint}))
                                 f (or (prs op)
                                       (-fail! ::missing-constraint-parser {:op op
                                                                            :constraint ?constraint}))]
                             (m/schema (if (or (nil? ?p) (map? ?p))
                                         (f {:properties ?p :children (when (< 2 n) (subvec ?constraint 2 n))} options)
                                         (f {:children (when (< 1 n) (subvec ?constraint 1 n))} options))
                                       options))
     :else (-fail! ::invalid-constraint {:outer-schema (-> options ::m/constraint-context :type)
                                         :constraint ?constraint}))))

(defn -constraint-from-properties [properties options]
  (let [{:keys [parse-properties]} (::m/constraint-context options)
        ;; important for deterministic m/explain ordering
        ks (-> parse-properties keys sort)]
    (when-some [cs (-> []
                       (into (keep #(when-some [[_ v] (find properties %)]
                                      (constraint ((get parse-properties %) v options) options)))
                             ks)
                       not-empty)]
      (case (count cs)
        0 (constraint [:true] options)
        1 (first cs)
        (constraint (into [:and] cs) options)))))

(defn -walk-leaf+constraints [schema walker path constraint {::m/keys [constraint-opts] :as options}]
  (when (m/-accept walker schema path options)
    (let [constraint' (when constraint
                        (let [constraint-walker (or (::constraint-walker options)
                                                    (reify m/Walker
                                                      (-accept [_ constraint _ _] constraint)
                                                      (-inner [this constraint path options] (m/-walk constraint this path options))
                                                      (-outer [_ constraint _ children _] (m/-set-children constraint children))))]
                          (m/-walk constraint constraint-walker (conj path ::constraint)
                                   (assoc options
                                          ::constraint-walker constraint-walker
                                          ;; enables constraints that contain schemas, e.g., [:string {:edn :int}]
                                          ::schema-walker walker))))
          schema (cond-> schema
                   ;; don't try and guess the 'unparsed' properties we don't need to.
                   ;; 
                   (and (some? constraint')
                        (not (identical? constraint constraint')))
                   (m/-update-properties (fn [properties]
                                           (let [{:keys [unparse-properties]} constraint-opts
                                                 f (or (get unparse-properties (m/type constraint'))
                                                       (-fail! ::cannot-unparse-constraint-into-properties
                                                               {:constraint constraint'}))]
                                             (f constraint' properties options)))))]
      (m/-outer walker schema path (m/-children schema) options))))

(defn default-parse-constraints []
  {:and (fn [{:keys [properties children]} opts]
          (into [::and nil] children))
   :true (fn [{:keys [properties children]} opts]
           (m/-check-children! :true properties children 0 0)
           [::true-constraint])})

(defn default-parse-properties []
  {:and (fn [v _] (into [:and] v))})

(defn default-unparse-properties []
  {::and
   (fn [c into-properties {{:keys [unparse-properties]} ::m/constraint-context :as opts}]
     (reduce (fn [into-properties c]
               (unparse-properties c into-properties opts))
             into-properties (m/children c)))
   ::true (fn [_ into-properties _] into-properties)})

(defn default-constraint-form []
  {::and (fn [c options] (into [:and] (map m/form) (m/children c)))
   ::true-constraint (fn [c options] [:true])})

(defn base-constraint-extensions []
  {:string {:-walk -walk-leaf+constraints
            :constraint-from-properties -constraint-from-properties
            :parse-constraint (into (default-parse-constraints)
                                    {:count (fn [{:keys [properties children]} opts]
                                              (m/-check-children! :count properties children 2 2)
                                              (into [::count-constraint properties] children))
                                     :max (fn [{:keys [properties children]} opts]
                                            (m/-check-children! :max properties children 1 1)
                                            [::count-constraint 0 (first children)])
                                     :min (fn [{:keys [properties children]} opts]
                                            (m/-check-children! :min properties children 1 1)
                                            [::count-constraint (first children) nil])
                                     :gen/max (fn [{:keys [properties children]} opts]
                                                (m/-check-children! :gen/max properties children 1 1)
                                                [::count-constraint {:gen/max (first children)} 0 nil])
                                     :gen/min (fn [{:keys [properties children]} opts]
                                                (m/-check-children! :gen/min properties children 1 1)
                                                [::count-constraint {:gen/min (first children)} 0 nil])})
            :constraint-form (into (default-constraint-form)
                                   {::count-constraint (fn [c options]
                                                         (let [[min-count max-count] (m/children c)
                                                               {gen-min :gen/min gen-max :gen/max} (m/properties c)
                                                               frms (cond-> []
                                                                      (pos? min-count) (conj [:min min-count])
                                                                      max-count (conj [:max max-count])
                                                                      (some-> gen-min pos?) (conj [:gen/min gen-min])
                                                                      gen-max (conj [:gen/max gen-max]))]
                                                           (case (count frms)
                                                             0 [:true]
                                                             1 (first frms)
                                                             (let [ps (cond-> nil
                                                                        (some-> gen-min pos?) (assoc :gen/min gen-min)
                                                                        gen-max (assoc :gen/max gen-max))]
                                                               (-> [:count]
                                                                   (cond-> ps (conj ps))
                                                                   (conj min-count max-count))))))})
            :parse-properties (into (default-parse-properties)
                                    {:count (fn [v opts]
                                              (if (nat-int? v)
                                                [:count v v]
                                                (into [:count] v)))
                                     :max (fn [v opts] [:max v])
                                     :min (fn [v opts] [:min v])
                                     :gen/max (fn [v opts] [:gen/max v])
                                     :gen/min (fn [v opts] [:gen/min v])})
            :unparse-properties (into (default-unparse-properties)
                                      {::count-constraint
                                       (fn [c into-properties _]
                                         (let [[cmin cmax] (m/children c)
                                               c-properties (m/properties c)]
                                           (cond-> into-properties
                                             cmax (update (if (::gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                                             (pos? cmin) (update (if (::gen c-properties) :gen/min :min) #(if % (max % cmin) cmin)))))})}})

(defn -constraint-form [constraint {{:keys [constraint-form]} ::m/constraint-context :as options}]
  (let [t (m/type constraint)
        f (or (get constraint-form t)
              (-fail! ::no-constraint-form {:type t}))]
    (f constraint options)))

(defn -count-constraint []
  (let [type ::count-constraint]
    ^{:type ::m/into-schema}
    (reify
      m/AST
      (-from-ast [parent ast options] (throw (ex-info "TODO" {})))
      m/IntoSchema
      (-type [_] type)
      (-type-properties [_])
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (m/-check-children! type properties children 2 2)
        (let [[min-count max-count] children
              ;; unclear if we want to enforce (<= min-count max-count)
              ;; it's a perfectly well formed constraint that happens to satisfy no values
              _ (when-not (or (nil? min-count)
                              (nat-int? min-count))
                  (-fail! ::count-constraint-min {:min min-count}))
              _ (when-not (or (nil? max-count)
                              (nat-int? max-count))
                  (-fail! ::count-constraint-max {:max max-count}))
              this (volatile! nil)
              form (delay (-constraint-form @this options))
              cache (m/-create-cache options)]
          (vreset!
            this
            ^{:type ::m/schema}
            (reify
              mcp/Constraint
              (-constraint? [_] true)
              (-intersect [_ that options']
                (when (= type (m/type that))
                  (let [{gen-min :gen/min gen-max :gen/max} properties
                        [min-count' max-count'] (m/children that)
                        {gen-min' :gen/min gen-max' :gen/max} (m/properties that)
                        gen-min (or (when (and gen-min gen-min') (cc/max gen-min gen-min')) gen-min gen-min')
                        gen-max (or (when (and gen-max gen-max') (cc/min gen-max gen-max')) gen-max gen-max')]
                    (m/-into-schema parent
                                    (cond-> {}
                                      gen-min (assoc :gen/min gen-min)
                                      gen-max (assoc :gen/max gen-max))
                                    [(cc/max min-count min-count')
                                     (or (when (and max-count max-count') (cc/min max-count max-count')) max-count max-count')]
                                    options))))
              m/AST
              (-to-ast [this _] (m/-to-value-ast this))
              m/Schema
              ;;TODO bounded counts
              ;; idea: [:string {:or [[:min 0] [:min 5]]}] could just count once somehow
              ;; as opposed to [:or [:string {:min 1}] [:string {:min 5}]] which would be more difficult?
              (-validator [_]
                (cond
                  (and min-count max-count) (if (= min-count max-count)
                                              #(= min-count (miu/-safe-count %))
                                              (if (<= min-count max-count)
                                                #(<= min-count (miu/-safe-count %) max-count)
                                                (fn [_] false)))
                  (pos? min-count) #(<= min-count (miu/-safe-count %))
                  max-count #(<= (miu/-safe-count %) max-count)
                  :else any?))
              (-explainer [this path]
                (let [pred (m/-validator this)]
                  (fn [x in acc]
                    (cond-> acc
                      (not (pred x))
                      ;; TODO humanize ::count-limits
                      (conj (miu/-error path in this x ::count-limits))))))
              ;; potentially useful for :orn, :xorn, :impliesn constraints?
              ;; [:string {:orn [[:small [:max 5]] [:large [:min 6]]]}]
              ;; => [:small "12345"]
              ;; => [:large "123456"]
              ;; [:string {:impliesn [[:at-least-5 [:max 5]] [:at-least-6 [:max 6]]]}]
              ;; => [:at-least-5 "12345"]
              ;; => [[:not :at-least-5] "123456"]
              ;; [:string {:iffn [[:max-5-left [:max 5]] [:max-5-right [:max 5]]]}]
              ;; => [:P "12345"]
              ;; => [:not-P "123456"]
              (-parser [this]
                (let [validator (m/-validator this)]
                  (fn [x] (if (validator x) x ::m/invalid))))
              (-unparser [this] (m/-parser this))
              (-transformer [this transformer method options] (-fail! ::constraints-cannot-be-transformed this))
              (-walk [this walker path options] (m/-walk-leaf this walker path options))
              (-properties [_] properties)
              (-options [_] options)
              (-children [_] children)
              (-parent [_] parent)
              (-form [_] @form)
              m/Cached
              (-cache [_] cache)
              m/LensSchema
              (-keep [_] (throw (ex-info "TODO" {})))
              (-get [_ _ default] (throw (ex-info "TODO" {})))
              (-set [this key _] (throw (ex-info "TODO" {}))))))))))

(defn -true-constraint []
  (let [type ::true-constraint]
    ^{:type ::m/into-schema}
    (reify
      m/AST
      (-from-ast [parent ast options] (throw (ex-info "TODO" {})))
      m/IntoSchema
      (-type [_] type)
      (-type-properties [_])
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (m/-check-children! type properties children 0 0)
        (let [this (volatile! nil)
              form (delay (-constraint-form @this options))
              cache (m/-create-cache options)]
          (vreset!
            this
            ^{:type ::m/schema}
            (reify
              mcp/Constraint
              (-constraint? [_] true)
              (-intersect [this that options] (when (= type that) this))
              m/AST
              (-to-ast [this _] (throw (ex-info "TODO" {})))
              m/Schema
              (-validator [_] any?)
              ;;TODO make explainer and hook it up to humanizer
              (-explainer [this path] (fn [x in acc] acc))
              (-parser [this] identity)
              (-unparser [this] identity)
              (-transformer [this transformer method options]
                (m/-intercepting (m/-value-transformer transformer this method options)))
              (-walk [this walker path options] (m/-walk-leaf this walker path options))
              (-properties [_] properties)
              (-options [_] options)
              (-children [_] children)
              (-parent [_] parent)
              (-form [_] @form)
              m/Cached
              (-cache [_] cache)
              m/LensSchema
              (-keep [_])
              (-get [_ _ default] default)
              (-set [this key _] (m/-fail! ::non-associative-constraint {:schema this, :key key})))))))))

(defn- -flatten-and [cs]
  (eduction (mapcat #(if (= ::and (m/type %))
                       (m/children %)
                       [%]))
            cs))

(defn- -intersect-common-constraints [cs]
  (->> cs
       (group-by m/type)
       (sort-by key)
       (into [] (mapcat (fn [[_ v]]
                          (case (count v)
                            1 (subvec v 0 1)
                            (let [[l r & nxt] v]
                              ;; if the first two intersect successfully, assume the rest do too
                              (if-some [in (mcp/-intersect l r nil)]
                                [(if nxt
                                   (reduce #(mcp/-intersect %1 %2 nil) in nxt)
                                   in)]
                                v))))))))

(defn -and-constraint []
  ^{:type ::m/into-schema}
  (let [type ::and]
    (reify m/IntoSchema
      (-type [_] type)
      (-type-properties [_])
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (let [children (m/-vmap #(constraint % options) children)
              ichildren (-> children -flatten-and -intersect-common-constraints)]
          (case (count ichildren)
            1 (first ichildren)
            (let [children ichildren
                  this (volatile! nil)
                  ;;FIXME use pretty constraint form
                  form (delay (-constraint-form @this options))
                  cache (m/-create-cache options)
                  ->parser (fn [f m] (let [parsers (m (m/-vmap f children))]
                                       #(reduce (fn [x parser] (miu/-map-invalid reduced (parser x))) % parsers)))]
              (vreset!
                this
                ^{:type ::m/schema}
                (reify
                  mcp/Constraint
                  (-constraint? [_] true)
                  (-intersect [_ that options]
                    (when (= type (m/type that))
                      (m/-into-schema parent properties (into children (m/children that)) options)))
                  m/Schema
                  (-validator [_]
                    (let [validators (m/-vmap m/-validator children)] (miu/-every-pred validators)))
                  (-explainer [_ path]
                    (let [explainers (m/-vmap (fn [[i c]] (m/-explainer c (conj path i))) (map-indexed vector children))]
                      (fn explain [x in acc] (reduce (fn [acc' explainer] (explainer x in acc')) acc explainers))))
                  (-parser [_] (->parser m/-parser seq))
                  (-unparser [_] (->parser m/-unparser rseq))
                  (-transformer [this transformer method options]
                    (m/-parent-children-transformer this children transformer method options))
                  (-walk [this walker path options] (m/-walk-indexed this walker path options))
                  (-properties [_] properties)
                  (-options [_] options)
                  (-children [_] children)
                  (-parent [_] parent)
                  (-form [_] @form)
                  m/Cached
                  (-cache [_] cache)
                  m/LensSchema
                  (-keep [_])
                  (-get [_ key default] (get children key default))
                  (-set [this key value] (m/-set-assoc-children this key value)))))))))))

(defn base-constraints []
  {::count-constraint (-count-constraint)
   ::and (-and-constraint)
   ::true-constraint (-true-constraint)})

(defn register-constraint-extensions! [extensions] (swap! m/constraint-extensions #(merge-with into % extensions)))

(let [base-ext! (delay (register-constraint-extensions! (base-constraint-extensions)))
      bc (delay (base-constraints))]
  (defn activate-base-constraints!
    ([] (mr/swap-default-registry! activate-base-constraints!))
    ([?registry]
     @base-ext! ;; hmm this will break the default registry if it doesn't also include (base-constraints)
     (mr/composite-registry @bc ?registry))))
