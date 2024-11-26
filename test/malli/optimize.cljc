;; demo
(ns malli.optimize
  (:require [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.solver :as solver]))

(defmulti -solution-validator (fn [solution options] (:type solution)))
(defmethod -solution-validator :number
  [{:keys [max-number min-number] :as solution} _]
  #(and (number? %)
        (<= (or min-number %) % (or max-number %))))
(defmethod -solution-validator :int
  [{:keys [max-number min-number] :as solution} _]
  #(and (int? %)
        (<= (or min-number %) % (or max-number %))))

(defn- -validate-map-linearly
  [{keys-solutions :keys vals-solutions :vals get-solutions :get
    :keys [keyset open-map min-count max-count default-keys default-vals]} options]
  (let [required-keys (into #{} (mapcat (fn [[k v]] (when (= :present v) [k]))) keyset)
        forbidden-keys (into #{} (mapcat (fn [[k v]] (when (= :absent v) [k]))) keyset)
        default-validator (if open-map
                            (fn [_ k v]
                              (if (contains? forbidden-keys k)
                                (reduced false)
                                (assert nil)
                                ))
                            (if (or default-vals default-keys)
                              (assert nil)
                              ))
        get-validators (into {} (map (fn [[k s]]
                                       (let [valid? (-solution-validator s options)]
                                         (fn [state _ v]
                                           (if (valid? v)
                                             (when state (not-empty (disj state k)))
                                             (reduced false))))))
                             get-solutions)]
    (miu/-every-pred
      (-> [map?]
          (cond->
            (or min-count max-count) (conj (comp
                                             (miu/-every-pred
                                               (cond-> []
                                                 min-count #(<= min-count %)
                                                 max-count #(<= % max-count)))
                                             count)))
          (conj #(let [state (volatile! required-keys)]
                   (nil? (reduce-kv (fn [_ k v]
                                      ((get-validators k default-validator) state k v))
                                    required-keys %))))))))

(defn- -validate-map-via-lookup
  [{keys-solutions :keys vals-solutions :vals get-solutions :get
    :keys [keyset open-map min-count max-count default-keys default-vals]} options]
  (assert nil "TODO"))

;;hmm unclear which validator to use between keys vs default-keys
(defmethod -solution-validator :map
  [{get-solutions :get :keys [keyset open-map min-count max-count keys vals default-keys default-vals] :as solution} options]
  (if open-map
    (-validate-map-linearly solution options)
    (-validate-map-via-lookup solution options)))

(defmethod -solution-validator nil [_ _] any?)

(defn validator
  ([?schema] (validator ?schema nil))
  ([?schema options]
   (let [schema (m/schema ?schema options)
         solutions (solver/solve schema options)
         ors (mapv #(-solution-validator % options) solutions)]
     (miu/-some-pred ors))))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value options]
   ((validator ?schema options) value)))

(comment
  (validator number?)
  (assert (validate [:and number? [:<= 10]] 0))
  (assert (not (validate [:and number? [:<= 10]] 20)))
  (assert (validate [:int {:gen/max 10}] 20))
  (assert (not (validate [:int {:max 10}] 20)))
  (validate :map 20)
  )
