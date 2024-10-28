(ns malli.constraint
  (:require [clojure.core :as cc]
            [clojure.set :as set]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint.util :as mcu]
            [malli.constraint.ext.string :as string-ext]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr])
  #?(:clj (:import (clojure.lang IPersistentVector))))

(defn base-constraint-extensions []
  (merge (string-ext/base-constraint-extensions)))

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
              form (delay (mcu/-constraint-form @this options))
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
              form (delay (mcu/-constraint-form @this options))
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
        (let [children (m/-vmap #(mcu/constraint % options) children)
              ichildren (-> children -flatten-and -intersect-common-constraints)]
          (case (count ichildren)
            1 (first ichildren)
            (let [children ichildren
                  this (volatile! nil)
                  ;;FIXME use pretty constraint form
                  form (delay (mcu/-constraint-form @this options))
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

(let [base-ext! (delay (mcu/register-constraint-extensions! (base-constraint-extensions)))
      bc (delay (base-constraints))]
  (defn activate-base-constraints!
    ([] (mr/swap-default-registry! activate-base-constraints!))
    ([?registry]
     @base-ext! ;; hmm this will break the default registry if it doesn't also include (base-constraints)
     (mr/composite-registry @bc ?registry))))
