(ns malli.shrinker
  (:require [malli.core :as m]))

(defmulti -divider
  (fn [schema opts] (m/type schema))
  :default ::default)

(defmethod -divider ::default [_ _] (fn [_ _]))
(defmethod -divider :tuple [schema opts]
  (let [cs (m/children schema)]
    (fn [v path]
      (map-indexed (fn [i v]
                     {:schema (nth cs i)
                      :path (conj path i)
                      :vals [v]})
                   v))))

(defn -seq-parts [schema opts]
  (let [[c] (m/children schema)]
    (fn [v path]
      [{:schema c
        :path (conj path 0)
        :vals v}])))

(defmethod -divider :set [schema opts] (-seq-parts schema opts))
(defmethod -divider :sequential [schema opts] (-seq-parts schema opts))
(defmethod -divider :seqable [schema opts] (-seq-parts schema opts))
(defmethod -divider :every [schema opts] (-seq-parts schema opts))

(defmethod -divider :any [schema opts]
  (fn [v path]
    (when (coll? v)
      [{:schema schema
        :path path
        :vals (cond->> v
                (map? v) (apply concat))}])))

(defmethod -divider :map-of [schema opts]
  (let [[ks vs] (m/children schema)]
    (fn [v path]
      (when-some [m (seq v)]
        [{:schema ks
          :path (conj path 0)
          :vals (keys m)}
         {:schema vs
          :path (conj path 1)
          :vals (vals m)}]))))

(defmethod -divider :orn [schema opts]
  (let [parse (m/parser schema opts)
        child-dividers (into {}
                             (map (fn [[k _ s]]
                                    [k (fn [v]
                                         ;;TODO cache by eagerly tying knot
                                         ((-divider s opts) v))]))
                             (m/children schema))]
    (fn [v path]
      (let [p (parse v)
            _ (assert (not= ::m/invalid p))
            {:keys [key value]} p]
        ((child-dividers key) value path)))))

;; public API

(defn divider
  ([?schema] (divider ?schema nil))
  ([?schema opts] (-divider (m/schema ?schema opts) opts)))

(defn divide
  "Divide a value conforming to ?schema into a sequence
  of maps representing the children of the schema/value."
  ([?schema value] (divide ?schema value nil))
  ([?schema value opts] ((divider ?schema opts) value [])))

(defn shrinker
  "Takes a schema and
  returns a seq of divider parts of value that
  still conform to the overall schema."
  [?schema opts]
  (let [schema (m/deref-all (m/schema ?schema opts))
        parse (m/parser schema opts)
        unparse (m/unparser schema opts)]
    (fn [value]
      (let [p (parse value)
            _ (assert (not= p ::m/invalid))]
        ))))

(defn shrink
  "Takes a schema and a value conforming to it,
  returns a seq of divider parts of value that
  still conform to the overall schema."
  ([?schema value] (shrink ?schema value nil))
  ([?schema value opts]
   ((shrinker ?schema opts) value)))
