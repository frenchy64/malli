(ns malli.constraint.and
  (:require [clojure.core :as cc]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint.util :as mcu]
            [malli.constraint :as-alias mc]
            [malli.constraint.ext.string :as string-ext]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr]))

(defn- -flatten-and [cs]
  (eduction (mapcat #(if (= ::mc/and (m/type %))
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
  (let [type ::mc/and]
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
