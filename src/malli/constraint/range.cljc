(ns malli.constraint.range
  (:require [clojure.core :as cc]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint.util :as mcu]
            [malli.constraint :as-alias mc]
            [malli.constraint.ext.string :as string-ext]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr]))

(defn -range-constraint []
  (let [type ::mc/range-constraint]
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
        (let [[min-range max-range] children
              ;; unclear if we want to enforce (<= min-range max-range)
              ;; it's a perfectly well formed constraint that happens to satisfy no values
              _ (when-not (or (nil? min-range)
                              (number? min-range))
                  (-fail! ::mc/range-constraint-min {:min min-range}))
              _ (when-not (or (nil? max-range)
                              (number? max-range))
                  (-fail! ::mc/range-constraint-max {:max max-range}))
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
                        [min-range' max-range'] (m/children that)
                        {gen-min' :gen/min gen-max' :gen/max} (m/properties that)
                        gen-min (or (when (and gen-min gen-min') (cc/max gen-min gen-min')) gen-min gen-min')
                        gen-max (or (when (and gen-max gen-max') (cc/min gen-max gen-max')) gen-max gen-max')]
                    (m/-into-schema parent
                                    (cond-> {}
                                      gen-min (assoc :gen/min gen-min)
                                      gen-max (assoc :gen/max gen-max))
                                    [(or (when (and min-range min-range') (cc/max min-range min-range')) min-range min-range')
                                     (or (when (and max-range max-range') (cc/min max-range max-range')) max-range max-range')]
                                    options))))
              m/AST
              (-to-ast [this _] (m/-to-value-ast this))
              m/Schema
              (-validator [_]
                (prn "range validator" min-range max-range)
                (cond
                  (and min-range max-range) (if (= min-range max-range)
                                              #(= min-range %)
                                              (if (<= min-range max-range)
                                                #(and (<= min-range %)
                                                      (<= % max-range))
                                                (fn [_] false)))
                  min-range #(<= min-range %)
                  max-range #(<= % max-range)
                  :else any?))
              (-explainer [this path]
                (let [pred (m/-validator this)]
                  (fn [x in acc]
                    (cond-> acc
                      (not (pred x))
                      (conj (miu/-error path in this x ::mc/range-limits))))))
              (-parser [this]
                (let [validator (m/-validator this)]
                  (fn [x] (if (validator x) x ::m/invalid))))
              (-unparser [this] (m/-parser this))
              (-transformer [this transformer method options] (-fail! ::mc/constraints-cannot-be-transformed this))
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
