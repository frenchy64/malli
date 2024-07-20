(ns malli.poly2
  (:refer-clojure :exclude [eval type -deref deref -lookup -key assert])
  #?(:cljs (:require-macros malli.core))
  (:require [clojure.walk :as walk]
            [clojure.core :as c]
            [malli.core :as m]
            [malli.impl.regex :as re]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.locally-nameless :as mln :refer [-instantiate-many]]
            [malli.poly-protocols :refer [AllSchema -bounds -inst]]))

(declare inst)

(defn -all-names [s]
  (map #(nth % 0) (-bounds s)))

(defn -fv [?schema options]
  (let [fvs (atom #{})
        inner (fn [this s p options]
                (case (m/type s)
                  :all (m/-walk s this p (update options ::bound-tvs into (-all-names s)))
                  (m/-walk s this p options)))]
    (inner (reify m/Walker
             (-accept [_ s _ _] s)
             (-inner [this s p options]
               (inner this s p options))
             (-outer [_ s p c {::keys [bound-tvs] :as options}]
               (case (m/type s)
                 ::local (let [id (first c)]
                           ;;TODO when id is simple-symbol?
                           (when-not (contains? bound-tvs id)
                             (swap! fvs conj (assoc (m/-properties s) :id id))))
                 ;;TODO think harder about :..
                 nil)
               s))
           (m/schema ?schema options)
           []
           (assoc options
                  ::m/walk-refs false
                  ::m/walk-schema-refs false
                  ::m/walk-entry-vals true
                  ::bound-tvs #{}))
    @fvs))

(defn- -all-binder-bounds [binder]
  (m/-vmap (fn [b]
             (if (simple-ident? b)
               {:kind :Schema
                :default :any
                :lower nil
                :upper :any}
               (if (and (vector? b)
                        (= 2 (count b))
                        (simple-ident? (first b)))
                 {:kind :Schema
                  :default (second b)
                  :lower nil
                  :upper (second b)}
                 (if (and (map? b)
                          (simple-ident? (:name b)))
                   (dissoc b :name)
                   (m/-fail! ::invalid-all-binder {:binder binder})))))
           binder))

(defn -all-binder-names [binder]
  (m/-vmap (fn [b]
             (if (simple-ident? b)
               b
               (if (and (vector? b)
                        (= 2 (count b))
                        (simple-ident? (first b)))
                 (first b)
                 (if (and (map? b)
                          (simple-ident? (:name b)))
                   (:name b)
                   (m/-fail! ::invalid-all-binder {:binder binder})))))
           binder))

(defn- -inst* [binder body insts options]
  (when-not (= (count insts)
               (count binder))
    (m/-fail! ::wrong-number-of-schemas-to-inst
              {:binder binder :schemas insts}))
  (-instantiate-many body insts options))

(defn -all-binder-defaults [binder]
  (mapv :default (-all-binder-bounds binder)))


(defn -all-schema [_]
  ^{:type ::m/into-schema}
  (reify m/IntoSchema
    (-type [_] :all)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children {::m/keys [function-checker] :as options}]
      (m/-check-children! :all properties children 2 2)
      (let [[binder body-syntax] children
            ;;TODO parse binder + kind
            self-inst (delay (inst [:all binder body-syntax] options))
            body' (reduce
                    (fn [s _]
                      (m/schema (mln/-scoped s) options))
                    (m/schema body-syntax
                              (update options :registry
                                      #(mr/composite-registry
                                         (into {} (map-indexed (fn [i n] [n (m/schema [::mln/b i] options)]))
                                               (rseq (-all-binder-names binder)))
                                         (or % {}))))
                    (range (count binder)))
            form (delay
                   (m/-create-form :all properties
                                   (assoc children 1
                                          (-> body'
                                              (-instantiate-many 
                                                (mapv (fn [n] (m/schema [::mln/f n] options))
                                                      (-all-binder-names binder))
                                                options)
                                              m/form))
                                   options))
            cache (m/-create-cache options)
            ->checker (if function-checker #(function-checker % options) (constantly nil))]
        ^{:type ::m/schema}
        (reify
          m/Schema
          (-validator [this]
            (if-let [checker (->checker this)]
              (let [validator (fn [x] (nil? (checker x)))]
                (fn [x] (and (ifn? x) (validator x))))
              ifn?))
          (-explainer [this path]
            (if-let [checker (->checker this)]
              (fn explain [x in acc]
                (if (not (ifn? x))
                  (conj acc (miu/-error path in this x))
                  (if-let [res (checker x)]
                    (conj acc (assoc (miu/-error path in this x) :check res))
                    acc)))
              (let [validator (m/-validator this)]
                (fn explain [x in acc]
                  (if-not (validator x) (conj acc (miu/-error path in this x)) acc)))))
          (-parser [this]
            (let [validator (m/-validator this)]
              (fn [x] (if (validator x) x ::m/invalid))))
          (-unparser [this] (m/-parser this))
          (-transformer [_ _ _ _])
          (-walk [this walker path options] (m/-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          AllSchema
          (-bounds [_] (-all-binder-bounds binder))
          (-inst [_ insts] (-inst* binder body' (mapv (fn [{:keys [kind]} x]
                                                        (let [x (m/schema x options)]
                                                          (case kind
                                                            :Schema (when (m/-regex-op? x)
                                                                      (m/-fail! ::regex-not-kind-schema)))
                                                          x))
                                                      (-all-binder-bounds binder)
                                                      (or (seq insts) (-all-binder-defaults binder)))
                                   options))
          m/FunctionSchema
          (-function-schema? [this] (m/-function-schema? @self-inst))
          (-function-schema-arities [this] (m/-function-schema-arities @self-inst))
          (-function-info [this] (m/-function-info @self-inst))
          (-instrument-f [schema props f options] (m/-instrument-f @self-inst props f options))
          m/Cached
          (-cache [_] cache)
          m/LensSchema
          (-keep [_])
          (-get [_ key default] (get children key default))
          (-set [this key value] (m/-set-assoc-children this key value))
          m/RefSchema
          (-ref [_])
          (-deref [_] @self-inst))))))

(defn inst
  "Instantiate an :all schema with a vector of schemas. If a schema
  is nil, its upper bound will be used. If ?schemas is nil or not provided, same as
  vector of nils. ?schemas-or-options are treated as options if map?, otherwise ?schemas."
  ([?all] (inst ?all nil nil))
  ([?all ?schemas-or-options] (let [options? (map? ?schemas-or-options)
                                    ?schemas (when-not options? ?schemas-or-options)
                                    options (when options? ?schemas-or-options)]
                                (inst ?all ?schemas options)))
  ([?all insts options] (-inst (m/schema ?all options) (mapv #(m/schema % options) insts))))

(defn schemas []
  (into (mln/schemas) {:all (-all-schema nil)}))

(comment
  (m/ast [:schema {:registry {::a :int}} ::a])
  (m/ast [:enum :foo :bar])
  )
