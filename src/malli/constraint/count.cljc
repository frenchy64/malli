(ns malli.constraint.count
  (:require [clojure.core :as cc]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint.util :as mcu]
            [malli.constraint :as-alias mc]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr]))

;; assumes :and and :true
(defn default-constraint-extensions []
  {:parse-constraint {;; [:max 5] => [::mc/count-constraint 0 5]
                      :max (fn [{:keys [properties children]} opts]
                             (m/-check-children! :max properties children 1 1)
                             [::mc/count-constraint 0 (first children)])
                      ;; [:min 5] => [::mc/count-constraint 5 nil]
                      :min (fn [{:keys [properties children]} opts]
                             (m/-check-children! :min properties children 1 1)
                             [::mc/count-constraint (first children) nil])
                      ;; [:gen/max 5] => [::mc/count-constraint {:gen/max 5} 0 nil]
                      :gen/max (fn [{:keys [properties children]} opts]
                                 (m/-check-children! :gen/max properties children 1 1)
                                 [::mc/count-constraint {:gen/max (first children)} 0 nil])
                      ;; [:gen/min 5] => [::mc/count-constraint {:gen/min 5} 0 nil]
                      :gen/min (fn [{:keys [properties children]} opts]
                                 (m/-check-children! :gen/min properties children 1 1)
                                 [::mc/count-constraint {:gen/min (first children)} 0 nil])}
   :constraint-form {::mc/count-constraint (fn [c options]
                                             (let [[min-count max-count] (m/children c)
                                                   {gen-min :gen/min gen-max :gen/max} (m/properties c)
                                                   frms (cond-> []
                                                          (pos? min-count) (conj [:min min-count])
                                                          max-count (conj [:max max-count])
                                                          (some-> gen-min pos?) (conj [:gen/min gen-min])
                                                          gen-max (conj [:gen/max gen-max]))]
                                               (case (count frms)
                                                 ;; [:string {:and [[:true]]}] <= [::mc/count-constraint 0 nil]
                                                 0 [:true]
                                                 ;; [:string {:and [[:min 5]]}] <= [::mc/count-constraint 5 nil]
                                                 ;; [:string {:and [[:max 4]]}] <= [::mc/count-constraint 0 4]
                                                 ;; [:string {:and [[:gen/min 5]]}] <= [::mc/count-constraint {:gen/min 5} 0 nil]
                                                 ;; [:string {:and [[:gen/max 4]]}] <= [::mc/count-constraint {:gen/max 4} 0 nil]
                                                 1 (first frms)
                                                 ;; [:string {:and [[:min 3] [:max 4] [:gen/min 1] [:gen/max 2]]]}]
                                                 ;; <= [::mc/count-constraint {:gen/min 1 :gen/max 2} 3 4]
                                                 (into [:and] frms))))}
   :parse-properties {;; (m/-get-constraint [:string {:max 5}]) => [:max 5]
                      :max (fn [v opts] [:max v])
                      ;; (m/-get-constraint [:string {:min 5}]) => [:min 5]
                      :min (fn [v opts] [:min v])
                      ;; (m/-get-constraint [:string {:gen/max 5}]) => [:gen/max 5]
                      :gen/max (fn [v opts] [:gen/max v])
                      ;; (m/-get-constraint [:string {:gen/min 5}]) => [:gen/min 5]
                      :gen/min (fn [v opts] [:gen/min v])}
   :unparse-properties {::mc/count-constraint
                        (fn [c into-properties _]
                          (let [[cmin cmax] (m/children c)
                                c-properties (m/properties c)]
                            (cond-> into-properties
                              ;; [:string {:max 4}] <= [::mc/count-constraint 0 4]
                              ;; :string <= [::mc/count-constraint 0 nil]
                              ;;FIXME what does ::mc/gen mean?
                              cmax (update (if (::mc/gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                              ;; [:string {:min 5}] <= [::mc/count-constraint 5 nil]
                              ;; :string <= [::mc/count-constraint 0 nil]
                              (pos? cmin) (update (if (::mc/gen c-properties) :gen/min :min) #(if % (max % cmin) cmin)))))}})

(defn -count-constraint []
  (let [type ::mc/count-constraint]
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
              _ (when-not (nat-int? min-count)
                  (-fail! ::mc/count-constraint-min {:min min-count}))
              _ (when-not (or (nil? max-count)
                              (nat-int? max-count))
                  (-fail! ::mc/count-constraint-max {:max max-count}))
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
                                                #(let [size (miu/-safe-count %)]
                                                   (and (<= min-count size)
                                                        (<= size max-count)))
                                                (fn [_] false)))
                  (pos? min-count) #(<= min-count (miu/-safe-count %))
                  max-count #(<= (miu/-safe-count %) max-count)
                  :else any?))
              (-explainer [this path]
                (let [pred (m/-validator this)]
                  (fn [x in acc]
                    (cond-> acc
                      (not (pred x))
                      (conj (miu/-error path in this x ::mc/count-limits))))))
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
