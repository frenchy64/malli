(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr])
  #?(:clj (:import (clojure.lang IPersistentVector))))

(defn -constraint-from-properties [properties options]
  (let [{:keys [parse-properties]} (::m/constraint-options options)
        ks (-> parse-properties keys sort)]
    (when-some [cs (-> []
                       (into (keep #(when-some [[_ v] (find properties %)]
                                      (constraint ((get parse-properties %) v options) options)))
                             ks)
                       not-empty)]
      (if (= 1 (count cs))
        (first cs)
        (constraint (into [::m/and-constraint] cs) options)))))

(defn constraint [?constraint options]
  (if (mcp/-constraint? ?constraint)
    ?constraint
    (if (vector? ?constraint)
      (let [v #?(:clj ^IPersistentVector ?constraint, :cljs ?constraint)
            n #?(:bb (count v) :clj (.count v), :cljs (count v))
            op #?(:clj (.nth v 0), :cljs (nth v 0))
            ?p (when (> n 1) #?(:clj (.nth v 1), :cljs (nth v 1)))
            prs (or (-> options ::m/constraint-options :parse-constraint)
                    (-fail! ::missing-parse-constraint-options {:constraint ?constraint}))
            f (or (prs op)
                  (-fail! ::missing-constraint-parser {:constraint ?constraint}))]
        (m/schema (if (or (nil? ?p) (map? ?p))
                    (f {:properties ?p :children (when (< 2 n) (subvec ?constraint 2 n))} options)
                    (f {:children (when (< 1 n) (subvec ?constraint 1 n))} options))
                  options))
      (-fail! ::invalid-constraint {:constraint ?constraint}))))

(comment
  (-constraint-from-properties
    {:max 1 :min 0}
    {::m/constraint-options (:string (base-constraint-extensions))})
  )

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


(defn base-constraint-extensions []
  {:string {:-walk -walk-leaf+constraints
            :constraint-from-properties -constraint-from-properties
            :parse-constraint {:max (fn [{:keys [properties children]} opts]
                                      (m/-check-children! :max properties children 1 1)
                                      [::count-constraint 0 (first children)])
                               :min (fn [{:keys [properties children]} opts]
                                      (m/-check-children! :min properties children 1 1)
                                      [::count-constraint (first children) nil])
                               :gen/max (fn [{:keys [properties children]} opts]
                                          (m/-check-children! :gen/max properties children 1 1)
                                          [::count-constraint {::gen true} 0 (first children)])
                               :gen/min (fn [{:keys [properties children]} opts]
                                          (m/-check-children! :gen/min properties children 1 1)
                                          [::count-constraint {::gen true} (first children) nil])
                               :and (fn [{:keys [properties children]} opts]
                                      (into [::and nil] children))}
            :constraint-form {::count-constraint (fn [c opts]
                                                   (let [[min max] (m/children c)]
                                                     (cond
                                                       (and min max) [:and [:min min] [:max max]]
                                                       min [:min min]
                                                       :else [:max max])))
                              ::and-constraint (fn [c opts]
                                                 ;; TODO flatten :and?
                                                 (into [:and] (m/children c)))}
            :parse-properties {:max (fn [v opts]
                                      [::count-constraint 0 v])
                               :min (fn [v opts]
                                      [::count-constraint v nil])
                               :gen/max (fn [v opts]
                                          [::count-constraint {::gen true} 0 v])
                               :gen/min (fn [v opts]
                                          [::count-constraint {::gen true} v nil])
                               :and (fn [vs opts]
                                      (into [::and nil] vs))}
            :unparse-properties {::count-constraint
                                 (fn [c into-properties opts]
                                   (let [[cmin cmax] (m/children c)
                                         c-properties (m/properties c)]
                                     (cond-> into-properties
                                       cmax (update (if (::gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                                       (pos? cmin) (update (if (::gen c-properties) :gen/min :min) #(if % (max % cmin) cmin)))))
                                 ::and-constraint
                                 (fn [c into-properties opts]
                                   (reduce (fn [into-properties]
                                             (throw (ex-info "TODO" {}))
                                             (or (::unparse-properties c into-properties opts)
                                                 ))
                                           into-properties))
                                 }}})

(defn -count-constraint []
  (let [type ::count-constraint]
    ^{:type ::m/into-schema}
    (reify
      m/AST
      (-from-ast [parent ast options] (throw (ex-info "TODO" {})))
      m/IntoSchema
      (-type [_] ::count-constraint)
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
              ;;TODO use :constraint-form
              form (delay (m/-simple-form parent properties children identity options))
              cache (m/-create-cache options)]
          ^{:type ::m/schema}
          (reify
            mcp/Constraint
            (-constraint? [_] true)
            m/AST
            (-to-ast [this _] (throw (ex-info "TODO" {})))
            m/Schema
            (-validator [_]
              ;;TODO bounded counts
              (cond
                (::gen properties) any?
                (and min-count max-count) (if (= min-count max-count)
                                            #(= min-count (miu/-safe-count %))
                                            #(<= min-count (miu/-safe-count %) max-count))
                min-count #(<= min-count (miu/-safe-count %))
                max-count #(<= (miu/-safe-count %) max-count)
                :else any?))
            (-explainer [this path] (-fail! ::constraints-cannot-have-explainers this))
            (-parser [this] (-fail! ::constraints-cannot-be-parsed this))
            (-unparser [this] (-fail! ::constraints-cannot-be-unparsed this))
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
            (-set [this key _] (throw (ex-info "TODO" {})))))))))

(defn -and-constraint [] (m/-and-schema {:constraint-type ::and}))

(defn base-constraints []
  {::count-constraint (-count-constraint)
   ::and-constraint (-and-constraint)})

(defn register-constraint-extensions! [extensions] (swap! m/constraint-extensions #(merge-with into % extensions)))

(let [base-ext! (delay (register-constraint-extensions! (base-constraint-extensions)))
      bc (delay (base-constraints))]
  (defn activate-base-constraints!
    ([] (mr/swap-default-registry! activate-base-constraints!))
    ([?registry]
     @base-ext! ;; hmm this will break the default registry if it doesn't also include (base-constraints)
     (mr/composite-registry @bc ?registry))))
