;; demo (not included in jar)
(ns malli.optimize
  (:require [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.solver :as solver]))

(set! *warn-on-reflection* true)
(declare -solutions-validator)

(defmulti -solution-validator (fn [solution options] (:type solution)))
(defn- -min-max-validator [min-number max-number]
  (when (or min-number max-number)
    (miu/-every-pred
      (into (if (= min-number max-number)
              [#(= min-number %)]
              (cond-> []
                min-number (conj #(<= min-number %))
                max-number (conj #(<= % max-number))))))))
(defmethod -solution-validator :number
  [{:keys [max-number min-number] :as solution} _]
  (let [mmv (-min-max-validator min-number max-number)]
    (miu/-every-pred
      (cond-> [number?]
        mmv (conj mmv)))))
(defmethod -solution-validator :int
  [{:keys [max-number min-number] :as solution} _]
  (let [mmv (-min-max-validator min-number max-number)]
    (miu/-every-pred
      (cond-> [int?]
        mmv (conj mmv)))))

(defn- -validate-map-linearly
  [{keys-solutions :keys vals-solutions :vals get-solutions :get
    :keys [keyset open-map min-count max-count default-keys default-vals]} options]
  (let [required-keys (not-empty (into #{} (mapcat (fn [[k v]] (when (= :present v) [k]))) keyset))
        nrequired (count required-keys)
        forbidden-keys (let [phs (into #{} (mapcat (fn [[k v]] (when (= :absent v) [k]))) keyset)]
                         #?(:clj (java.util.HashSet. ^java.util.Set phs)
                            :default phs))
        valid-key? (-solutions-validator (or keys-solutions [{}]) options)
        valid-val? (-solutions-validator (or vals-solutions [{}]) options)
        error-val (reduced -1)
        default-validator (fn [nrequired k v]
                            (if (#?(:clj .contains :default contains?) forbidden-keys k)
                              error-val
                              (if (valid-key? k)
                                (if (valid-val? v)
                                  nrequired
                                  error-val)
                                (if open-map
                                  ;;can't have default and be open but if you could this would pass
                                  nrequired
                                  error-val))))
        get-validators (let [phm (into {} (map (fn [[k s]]
                                                 (let [valid? (-solution-validator s options)
                                                       register-required (if (contains? required-keys k) dec identity)]
                                                   [k (fn [nrequired _ v]
                                                        (if (valid? v)
                                                          (register-required nrequired)
                                                          error-val))])))
                                       get-solutions)]
                         #?(:clj (java.util.HashMap. ^java.util.Map phm)
                            :default phm))]
    (miu/-every-pred
      (-> [map?]
          (cond->
            (or min-count max-count) (conj (comp (-min-max-validator min-count max-count) count)))
          (conj #(zero? (reduce-kv (fn [state k v]
                                     ((#?(:clj .getOrDefault) get-validators k default-validator) state k v))
                                   nrequired %)))))))

(defn- -validate-map-via-lookup
  [{keys-solutions :keys vals-solutions :vals get-solutions :get
    :keys [keyset open-map min-count max-count default-keys default-vals]} options]
  (assert nil "TODO"))

;;hmm unclear which validator to use between keys vs default-keys
(defmethod -solution-validator :map
  [{:keys [open-map] :as solution} options]
  (if true #_open-map
    (-validate-map-linearly solution options)
    (-validate-map-via-lookup solution options)))

(defmethod -solution-validator nil [_ _] any?)

(defn -solutions-validator [solutions options]
  (let [;;TODO group related types together
        ors (mapv #(-solution-validator % options) solutions)]
    (miu/-some-pred ors)))

(defn validator
  ([?schema] (validator ?schema nil))
  ([?schema options]
   (-> ?schema
       (solver/solve options)
       (-solutions-validator options))))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value options]
   ((validator ?schema options) value)))

