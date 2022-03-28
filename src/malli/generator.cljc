(ns malli.generator
  (:require [clojure.spec.gen.alpha :as ga]
            [clojure.string :as str]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [clojure.spec.gen.alpha :as ga]
            [malli.core :as m]
            #?(:clj [borkdude.dynaload :as dynaload])
            [malli.util :as mu]))

(declare generator generate -create)

(defn schema->scalar-schema
  "Replace recursive aliases with :never and simplify."
  [schema options]
  (mu/walk* schema ;; FIXME alpha renaming needed?
            (fn inner [schema path {::keys [seen-refs] :as options}]
              (cond
                (and (satisfies? m/RefSchema schema)
                     (some? (m/-ref schema)))
                (let [options (update options ::seen-refs (fnil conj #{}) (m/-ref schema))]
                  ;; FIXME does this handle ref shadowing correctly?
                  (if (contains? seen-refs (m/-ref schema))
                    [(m/schema :never) options]
                    (inner (m/deref schema) path options)))

                :else (let [dschema (m/deref schema)]
                        (if (= schema dschema)
                          [schema options]
                          (inner dschema path options)))))
            (fn [schema _path _children options]
              (m/-simplify schema))
            options))

(defn schema->container-schema
  "Return a schema with free variables for recursive refs."
  [schema options]
  (mu/walk*
    schema ;; FIXME alpha renaming needed?
    (fn inner [schema path {::keys [seen-refs] :as options}]
      (cond
        (and (satisfies? m/RefSchema schema)
             (not (seen-refs (m/-ref schema))))
        (let [options (cond-> options
                        (m/-ref schema) (update ::seen-refs conj (m/-ref schema)))]
          (inner (m/deref schema) path options))

        :else [schema options]))
    (fn [schema _path _children options]
      (m/-simplify schema))
    (assoc options
           ;; do not expand free variables that have already been identified as recursive
           ::seen-refs (set (keys (::rec-gen options)))
           ::m/walk-entry-vals true
           ::m/allow-invalid-refs false)))

(defprotocol Generator
  (-generator [this options] "returns generator for schema"))

;;
;; generators
;;

(defonce ^:private never-gen (gen/such-that (fn [_] (do (println `never-gen "BUG! Should never attempt to generate!") false))
                                            gen/any))
(defn- unreachable-gen? [g] (identical? never-gen g))
(defn- not-unreachable [g] (when-not (unreachable-gen? g) g))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn -recur [schema {::keys [recursion recursion-limit] :or {recursion-limit 4} :as options}]
  (let [form (m/form schema)
        i (get recursion form 0)]
    [(<= i recursion-limit) (update options ::recursion assoc form (inc i))]))

(defn -maybe-recur [schema options]
  (let [[recur options] (-recur schema options)]
    (when recur options)))

(defn -min-max [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)]
    (when (and min gen-min (< gen-min min))
      (m/-fail! ::invalid-property {:key :gen/min, :value gen-min, :min min}))
    (when (and max gen-max (> gen-max max))
      (m/-fail! ::invalid-property {:key :gen/max, :value gen-min, :max min}))
    {:min (or gen-min min)
     :max (or gen-max max)}))

(defn- -double-gen [options] (gen/double* (merge {:infinite? false, :NaN? false} options)))

(defn- -string-gen [schema options]
  (let [{:keys [min max]} (-min-max schema options)]
    (cond
      (and min (= min max)) (gen/fmap str/join (gen/vector gen/char min))
      (and min max) (gen/fmap str/join (gen/vector gen/char min max))
      min (gen/fmap str/join (gen/vector gen/char min (* 2 min)))
      max (gen/fmap str/join (gen/vector gen/char 0 max))
      :else gen/string-alphanumeric)))

(defn- -coll-gen [schema f options]
  (let [{:keys [min max]} (-min-max schema options)
        [continue options] (-recur schema options)
        child (-> schema m/children first)
        gen (when continue (generator child options))]
    (gen/fmap f (cond
                  ((some-fn not unreachable-gen?) gen) (gen/vector gen/any 0 0)
                  (and min (= min max)) (gen/vector gen min)
                  (and min max) (gen/vector gen min max)
                  min (gen/vector gen min (* 2 min))
                  max (gen/vector gen 0 max)
                  :else (gen/vector gen)))))

(defn- -coll-distinct-gen [schema f options]
  (let [{:keys [min max]} (-min-max schema options)
        [continue options] (-recur schema options)
        child (-> schema m/children first)
        gen (not-unreachable (when continue (generator child options)))]
    (gen/fmap f (if gen
                  (gen/vector-distinct gen {:min-elements min, :max-elements max, :max-tries 100})
                  (gen/vector gen/any 0 0)))))

(defn -or-gen [schema options]
  (if-some [gs (seq (keep #(some->> (-maybe-recur % options) (generator %) not-unreachable) (m/children schema options)))]
    (gen/one-of gs)
    never-gen))

(defn -multi-gen [schema options]
  (if-some [gs (seq (keep #(some->> (-maybe-recur (last %) options) (generator (last %)) not-unreachable) (m/entries schema options)))]
    (gen/one-of gs)
    never-gen))

(defn -map-gen [schema options]
  (let [entries (m/entries schema)
        [continue options] (-recur schema options)
        value-gen (fn [k s] (let [g (generator s options)]
                              (cond->> g
                                (not-unreachable g)
                                (gen/fmap (fn [v] [k v])))))
        gens-req (->> entries
                      (remove #(-> % last m/properties :optional))
                      (map (fn [[k s]] (value-gen k s))))
        gen-opt (->> entries
                     (filter #(-> % last m/properties :optional))
                     (map (fn [[k s]] (let [g (not-unreachable (when continue (value-gen k s)))]
                                        (gen/one-of (cond-> [(gen/return nil)]
                                                      g (conj g))))))
                     (apply gen/tuple))]
    (if (not-any? unreachable-gen? gens-req)
      (gen/fmap (fn [[req opt]] (into {} (concat req opt))) (gen/tuple (apply gen/tuple gens-req) gen-opt))
      never-gen)))

(defn -map-of-gen [schema options]
  (let [{:keys [min max]} (-min-max schema options)
        [k-gen v-gen] (map #(generator % options) (m/children schema options))
        opts (cond
               (and min (= min max)) {:num-elements min}
               (and min max) {:min-elements min :max-elements max}
               min {:min-elements min}
               max {:max-elements max}
               :else {})]
    (if ((some-fn unreachable-gen?) k-gen v-gen)
      (if (= 0 (or min 0) (or max 0))
        (gen/return {})
        never-gen)
      (gen/fmap #(into {} %) (gen/vector-distinct (gen/tuple k-gen v-gen) opts)))))

#?(:clj
   (defn -re-gen [schema options]
     ;; [com.gfredericks/test.chuck "0.2.10"+]
     (if-let [string-from-regex @(dynaload/dynaload 'com.gfredericks.test.chuck.generators/string-from-regex {:default nil})]
       (let [re (or (first (m/children schema options)) (m/form schema options))]
         (string-from-regex (re-pattern (str/replace (str re) #"^\^?(.*?)(\$?)$" "$1"))))
       (m/-fail! :test-chuck-not-available))))

(defn -ref-gen [schema options]
  (let [ref-name (m/-ref schema)]
    (assert ref-name)
    (or 
      ;; already seen this :ref, the generator is handy and we're done
      (get-in options [::rec-gen ref-name])
      ;; otherwise, continue to unroll but save the generator for this ref for later
      (let [container-schema (schema->container-schema schema options)
            fvs (mu/schema-fvs container-schema)]
        ;(prn container-schema fvs)
        (if-not (contains? fvs ref-name)
          ;; this ref is not recursive, does not need special handling
          (generator (m/deref schema) options)
          (gen/recursive-gen
            (fn [ref-gen]
              (generator container-schema
                         (-> options
                             (assoc-in [::rec-gen ref-name] ref-gen)
                             (update ::seen-refs (fnil conj #{}) ref-name))))
            (generator (schema->scalar-schema schema options) options)))))))

(defn -=>-gen [schema options]
  (let [output-generator (generator (:output (m/-function-info schema)) options)]
    (gen/return (m/-instrument {:schema schema} (fn [& _] (generate output-generator options))))))

(defn -function-gen [schema options]
  (gen/return (m/-instrument {:schema schema, :gen #(generate % options)} options)))

(defn -regex-generator [schema options]
  (if (m/-regex-op? schema)
    (generator schema options)
    (gen/tuple (generator schema options))))

(defn- entry->schema [e] (if (vector? e) (get e 2) e))

(defn -cat-gen [schema options]
  (->> (m/children schema options)
       (map #(-regex-generator (entry->schema %) options))
       (apply gen/tuple)
       (gen/fmap #(apply concat %))))

(defn -alt-gen [schema options]
  (gen/one-of (keep (fn [e]
                      (let [child (entry->schema e)]
                        (some->> (-maybe-recur child options) (-regex-generator child))))
                    (m/children schema options))))

(defn -?-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if (m/-regex-op? child)
      (gen/one-of [(generator child options) (gen/return ())])
      (gen/vector (generator child options) 0 1))))

(defn -*-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if (m/-regex-op? child)
      (gen/fmap #(apply concat %) (gen/vector (generator child options)))
      (gen/vector (generator child options)))))

(defn -repeat-gen [schema options]
  (let [child (m/-get schema 0 nil)]
    (if (m/-regex-op? child)
      (gen/fmap #(apply concat %) (-coll-gen schema identity options))
      (-coll-gen schema identity options))))

(defn -qualified-ident-gen [schema mk-value-with-ns value-with-ns-gen-size pred gen]
  (if-let [namespace-unparsed (:namespace (m/properties schema))]
    (gen/fmap (fn [k] (mk-value-with-ns (name namespace-unparsed) (name k))) value-with-ns-gen-size)
    (gen/such-that pred gen)))

(defn -qualified-keyword-gen [schema]
  (-qualified-ident-gen schema keyword gen/keyword qualified-keyword? gen/keyword-ns))

(defn -qualified-symbol-gen [schema]
  (-qualified-ident-gen schema symbol gen/symbol qualified-symbol? gen/symbol-ns))

(defmulti -schema-generator (fn [schema options] (m/type schema options)) :default ::default)

(defmethod -schema-generator ::default [schema options] (ga/gen-for-pred (m/validator schema options)))

(defmethod -schema-generator :> [schema options] (-double-gen {:min (-> schema (m/children options) first inc)}))
(defmethod -schema-generator :>= [schema options] (-double-gen {:min (-> schema (m/children options) first)}))
(defmethod -schema-generator :< [schema options] (-double-gen {:max (-> schema (m/children options) first dec)}))
(defmethod -schema-generator :<= [schema options] (-double-gen {:max (-> schema (m/children options) first)}))
(defmethod -schema-generator := [schema options] (gen/return (first (m/children schema options))))
(defmethod -schema-generator :not= [schema options] (gen/such-that #(not= % (-> schema (m/children options) first)) gen/any-printable 100))
(defmethod -schema-generator 'pos? [_ _] (gen/one-of [(-double-gen {:min 0.00001}) gen/s-pos-int]))
(defmethod -schema-generator 'neg? [_ _] (gen/one-of [(-double-gen {:max -0.0001}) gen/s-neg-int]))

(defmethod -schema-generator :not [schema options] (gen/such-that (m/validator schema options) (ga/gen-for-pred any?) 100))
(defmethod -schema-generator :and [schema options] (gen/such-that (m/validator schema options) (-> schema (m/children options) first (generator options)) 100))
(defmethod -schema-generator :or [schema options] (-or-gen schema options))
(defmethod -schema-generator :orn [schema options] (-or-gen (m/into-schema :or (m/properties schema) (map last (m/children schema)) (m/options schema)) options))
(defmethod -schema-generator ::m/val [schema options] (generator (first (m/children schema)) options))
(defmethod -schema-generator :map [schema options] (-map-gen schema options))
(defmethod -schema-generator :map-of [schema options] (-map-of-gen schema options))
(defmethod -schema-generator :multi [schema options] (-multi-gen schema options))
(defmethod -schema-generator :vector [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :sequential [schema options] (-coll-gen schema identity options))
(defmethod -schema-generator :set [schema options] (-coll-distinct-gen schema set options))
(defmethod -schema-generator :enum [schema options] (gen/elements (m/children schema options)))

(defmethod -schema-generator :maybe [schema options]
  (let [[continue options] (-recur schema options)]
    (gen/one-of (into [(gen/return nil)] (when continue [(-> schema (m/children options) first (generator options))])))))

(defmethod -schema-generator :tuple [schema options] (apply gen/tuple (mapv #(generator % options) (m/children schema options))))
#?(:clj (defmethod -schema-generator :re [schema options] (-re-gen schema options)))
(defmethod -schema-generator :any [_ _] (ga/gen-for-pred any?))
(defmethod -schema-generator :nil [_ _] (gen/return nil))
(defmethod -schema-generator :string [schema options] (-string-gen schema options))
(defmethod -schema-generator :int [schema options] (gen/large-integer* (-min-max schema options)))
(defmethod -schema-generator :double [schema options]
  (gen/double* (merge (let [props (m/properties schema options)]
                        {:infinite? (get props :gen/infinite? false)
                         :NaN? (get props :gen/NaN? false)})
                      (-min-max schema options))))
(defmethod -schema-generator :boolean [_ _] gen/boolean)
(defmethod -schema-generator :keyword [_ _] gen/keyword)
(defmethod -schema-generator :symbol [_ _] gen/symbol)
(defmethod -schema-generator :qualified-keyword [schema _] (-qualified-keyword-gen schema))
(defmethod -schema-generator :qualified-symbol [schema _] (-qualified-symbol-gen schema))
(defmethod -schema-generator :uuid [_ _] gen/uuid)

(defmethod -schema-generator :=> [schema options] (-=>-gen schema options))
(defmethod -schema-generator :function [schema options] (-function-gen schema options))
(defmethod -schema-generator 'ifn? [_ _] gen/keyword)
(defmethod -schema-generator :ref [schema options] (-ref-gen schema options))
(defmethod -schema-generator :schema [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator ::m/schema [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :merge [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :union [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :select-keys [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :cat [schema options] (-cat-gen schema options))
(defmethod -schema-generator :catn [schema options] (-cat-gen schema options))
(defmethod -schema-generator :alt [schema options] (-alt-gen schema options))
(defmethod -schema-generator :altn [schema options] (-alt-gen schema options))

(defmethod -schema-generator :? [schema options] (-?-gen schema options))
(defmethod -schema-generator :* [schema options] (-*-gen schema options))
(defmethod -schema-generator :+ [schema options] (gen/not-empty (-*-gen schema options)))
(defmethod -schema-generator :repeat [schema options] (-repeat-gen schema options))

;;
;; Creating a generator by different means, centralized under [[-create]]
;;

(defn- -create-from-elements [props]
  (some-> (:gen/elements props) gen/elements))

(defn- -create-from-gen
  [props schema options]
  (or (:gen/gen props)
      (when-not (:gen/elements props)
        (if (satisfies? Generator schema)
          (-generator schema options)
          (-schema-generator schema options)))))

(defn- -create-from-schema [props options]
  (some-> (:gen/schema props) (generator options)))

(defn- -create-from-fmap [props schema options]
  (when-some [fmap (:gen/fmap props)]
    (gen/fmap (m/eval fmap (or options (m/options schema)))
              (or (-create-from-elements props)
                  (-create-from-schema props options)
                  (-create-from-gen props schema options)
                  (gen/return nil)))))

(defn- -create [schema options]
  (let [props (merge (m/type-properties schema)
                     (m/properties schema))]
    (or (-create-from-fmap props schema options)
        (-create-from-elements props)
        (-create-from-schema props options)
        (-create-from-gen props schema options)
        (m/-fail! ::no-generator {:options options
                                  :schema schema}))))

;;
;; public api
;;

(defn generator
  ([?schema]
   (generator ?schema nil))
  ([?schema options]
   (m/-cached (m/schema ?schema options) :generator #(-create % options))))

(defn generate
  ([?gen-or-schema]
   (generate ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 30} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (rose/root (gen/call-gen gen (-random seed) size)))))

(defn sample
  ([?gen-or-schema]
   (sample ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 10} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (->> (gen/make-size-range-seq size)
          (map #(rose/root (gen/call-gen gen %1 %2))
               (gen/lazy-random-states (-random seed)))
          (take size)))))

;;
;; functions
;;

(defn function-checker
  ([?schema] (function-checker ?schema nil))
  ([?schema {::keys [=>iterations] :or {=>iterations 100} :as options}]
   (let [schema (m/schema ?schema options)
         check (fn [schema]
                 (let [{:keys [input output]} (m/-function-info schema)
                       input-generator (generator input options)
                       output-validator (m/validator output options)
                       validate (fn [f args] (output-validator (apply f args)))]
                   (fn [f]
                     (let [{:keys [result shrunk]} (->> (prop/for-all* [input-generator] #(validate f %))
                                                        (check/quick-check =>iterations))
                           smallest (-> shrunk :smallest first)]
                       (when-not (true? result)
                         (let [explain-input (m/explain input smallest)
                               response (when-not explain-input
                                          (try (apply f smallest) (catch #?(:clj Exception, :cljs js/Error) e e)))
                               explain-output (when-not explain-input (m/explain output response))]
                           (cond-> shrunk
                             explain-input (assoc ::explain-input explain-input)
                             explain-output (assoc ::explain-output explain-output)
                             (ex-message result) (-> (update :result ex-message)
                                                     (dissoc :result-data)))))))))]
     (condp = (m/type schema)
       :=> (check schema)
       :function (let [checkers (map #(function-checker % options) (m/-children schema))]
                   (fn [x] (->> checkers (keep #(% x)) (seq))))
       (m/-fail! ::invalid-function-schema {:type (m/-type schema)})))))

(defn check
  ([?schema f] (check ?schema f nil))
  ([?schema f options]
   (let [schema (m/schema ?schema options)]
     (m/explain (m/-update-options schema #(assoc % ::m/function-checker function-checker)) f))))
