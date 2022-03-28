(ns malli.generator-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.json-schema-test :as json-schema-test]
            [malli.util :as mu]))

(comment
  ;; test sizing
  (dotimes [size-pow 7]
    (time
      (prn
        (str "10^" size-pow)
        (into (sorted-map)
              (frequencies
                (map
                  (comp count flatten)
                  (mg/sample
                    [:schema {:registry {::cons [:maybe [:tuple pos-int? [:ref ::cons]]]}}
                     ::cons]
                    {:size (Math/pow 10 size-pow)})))))))
  ; (out) "10^0" {3 1}
  ; (out) "Elapsed time: 2.030981 msecs"
  ; (out) "10^1" {0 6, 2 3, 3 1}
  ; (out) "Elapsed time: 1.525037 msecs"
  ; (out) "10^2" {0 58, 2 28, 3 10, 4 4}
  ; (out) "Elapsed time: 3.935027 msecs"
  ; (out) "10^3" {0 497, 2 275, 3 138, 4 70, 5 17, 6 3}
  ; (out) "Elapsed time: 28.227256 msecs"
  ; (out) "10^4" {0 4968, 2 2789, 3 1338, 4 636, 5 218, 6 40, 7 9, 8 2}
  ; (out) "Elapsed time: 288.117389 msecs"
  ; (out) "10^5" {0 50219, 2 27130, 3 13152, 4 6214, 5 2484, 6 658, 7 121, 8 19, 9 3}
  ; (out) "Elapsed time: 2909.440828 msecs"
  ; (out) "10^6" {0 500465, 2 272651, 3 129733, 4 62264, 5 25107, 6 7724, 7 1759, 8 267, 9 29, 10 1}
  ; (out) "Elapsed time: 28400.642903 msecs"
  )

(deftest generator-test
  (doseq [[?schema _ ?fn] json-schema-test/expectations
          ;; cljs doesn't have a regex generator :(
          #?@(:cljs [:when (not= (m/type ?schema) :re)])
          :let [f (if ?fn #(%) identity)]]
    (testing (m/form ?schema)
      (testing "generate"
        (is (= (f (mg/generate ?schema {:seed 123}))
               (f (mg/generate ?schema {:seed 123}))))
        (is (= (f (mg/generate ?schema {:seed 123, :size 10}))
               (f (mg/generate ?schema {:seed 123, :size 10}))))
        (is (m/validate ?schema (mg/generate ?schema {:seed 123}))))
      (testing "sample"
        (is (= (map f (mg/sample ?schema {:seed 123}))
               (map f (mg/sample ?schema {:seed 123}))))
        (is (= (map f (mg/sample ?schema {:seed 123, :size 10}))
               (map f (mg/sample ?schema {:seed 123, :size 10}))))
        (doseq [value (mg/sample ?schema {:seed 123})]
          (is (m/validate ?schema value))))))

  (testing "simple schemas"
    (doseq [schema [:any
                    [:string {:min 1, :max 4}]
                    [:int {:min 1, :max 4}]
                    [:double {:min 0.0, :max 1.0}]
                    :boolean
                    :keyword
                    :symbol
                    :qualified-keyword
                    :qualified-symbol]]
      (is (every? (partial m/validate schema) (mg/sample schema {:size 1000})))))

  (testing "double properties"
    (let [infinity? #(or (= % ##Inf)
                         (= % ##-Inf))
          NaN? (fn [x]
                 (#?(:clj  Double/isNaN
                     :cljs js/isNaN)
                  x))
          special? #(or (NaN? %)
                        (infinity? %))
          test-presence (fn [f options]
                          (some f (mg/sample [:double options]
                                             {:size 1000})))]
      (is (test-presence infinity? {:gen/infinite? true}))
      (is (test-presence NaN? {:gen/NaN? true}))
      (is (test-presence special? {:gen/infinite? true
                                   :gen/NaN? true}))
      (is (not (test-presence special? nil)))))

  (testing "qualified-keyword properties"
    (testing "no namespace => random"
      (is (< 1 (->> (mg/sample [:qualified-keyword {:namespace nil}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count))))
      (is (< 1 (->> (mg/sample [:qualified-keyword {}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count)))))
    (testing "namespace => keyword with exact namesapce"
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-keyword {:namespace :hi}]
                             {:size 100})
                  (map namespace)
                  frequencies)))
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-keyword {:namespace "hi"}]
                             {:size 100})
                  (map namespace)
                  frequencies))))
    (testing "generated result should pass validation"
      (is (->> (mg/sample [:qualified-keyword {:namespace "hi"}]
                          {:size 100})
               (remove (partial m/validate [:qualified-keyword {:namespace "hi"}]))
               empty?))))

  (testing "qualified-symbol properties"
    (testing "no namespace => random"
      (is (< 1 (->> (mg/sample [:qualified-symbol {:namespace nil}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count))))
      (is (< 1 (->> (mg/sample [:qualified-symbol {}]
                               {:size 100})
                    (map namespace)
                    frequencies
                    (count)))))
    (testing "namespace => symbol with exact namesapce"
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-symbol {:namespace :hi}]
                             {:size 100})
                  (map namespace)
                  frequencies)))
      (is (= {"hi" 100}
             (->> (mg/sample [:qualified-symbol {:namespace "hi"}]
                             {:size 100})
                  (map namespace)
                  frequencies))))
    (testing "generated result should pass validation"
      (is (->> (mg/sample [:qualified-symbol {:namespace "hi"}]
                          {:size 100})
               (remove (partial m/validate [:qualified-symbol {:namespace "hi"}]))
               empty?))))

  (testing "map entries"
    (is (= {:korppu "koira"
            :piilomaan "pikku aasi"
            :muuli "mukkelis"}
           (mg/generate [:map {:gen/fmap '#(assoc % :korppu "koira")}
                         [:piilomaan {:gen/fmap '(partial str "pikku ")} [:string {:gen/elements ["aasi"]}]]
                         [:muuli {:gen/elements ["mukkelis"]} [:string {:gen/elements ["???"]}]]]))))

  (testing "ref"
    (testing "recursion"
      (let [schema [:schema {:registry {::cons [:maybe [:tuple int? [:ref ::cons]]]}}
                    ::cons]]
        (is (every? (partial m/validate schema) (mg/sample schema {:size 100})))))
    (testing "mutual recursion"
      (let [schema [:schema
                    {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                                ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                    ::ping]]
        (is (every? (partial m/validate schema) (mg/sample schema {:size 100})))))
    (testing "recursion limiting"
      (are [schema]
        (is (every? (partial m/validate schema) (mg/sample schema {:size 100})))

        [:schema {:registry {::rec [:maybe [:ref ::rec]]}} ::rec]
        [:schema {:registry {::rec [:map [:rec {:optional true} [:ref ::rec]]]}} ::rec]
        [:schema {:registry {::tuple [:tuple boolean? [:ref ::or]]
                             ::or [:or int? ::tuple]}} ::or]
        [:schema {:registry {::rec [:tuple int? [:vector {:max 2} [:ref ::rec]]]}} ::rec]
        [:schema {:registry {::rec [:tuple int? [:set {:max 2} [:ref ::rec]]]}} ::rec]
        [:schema {:registry {::multi
                             [:multi {:dispatch :type}
                              [:int [:map [:type [:= :int]] [:int int?]]]
                              [:multi [:map [:type [:= :multi]] [:multi {:optional true} [:ref ::multi]]]]]}} ::multi])))

  #?(:clj (testing "regex"
            (let [re #"^\d+ \d+$"]
              (m/validate re (mg/generate re)))

            (let [re-test #"(?=.{8,})" ;; contains unsupported feature
                  elements ["abcdefgh" "01234567"]
                  fmap '(fn [s] (str "prefix_" s))]
              (is (thrown-with-msg? Exception #"Unsupported-feature" (mg/generator [:re re-test])))
              (m/validate #".{8,}" (mg/generate [:re {:gen/elements elements} re-test]))
              (m/validate #"prefix_.{8,}" (mg/generate [:re {:gen/fmap fmap, :gen/elements elements} re-test])))))

  (testing "regex with custom generators"
    (is (= 42 (mg/generate [:re
                            {:gen/gen (gen/return 42)}
                            #"abc"]))
        "Using :gen/gen")
    (is (= 42 (mg/generate [:re
                            {:gen/fmap (fn [_] 42)
                             :gen/schema :int}
                            #"abc"]))
        "Using :gen/fmap and :gen/schema")
    (is (= 42 (mg/generate [:re
                            {:gen/elements [42]}
                            #"abc"]))
        "Using :gen/elements"))

  (testing "no generator"
    (is (thrown-with-msg?
         #?(:clj Exception, :cljs js/Error)
         #":malli.generator/no-generator"
         (mg/generate [:fn '(fn [x] (<= 0 x 10))]))))

  (testing "sci not available"
    (let [schema (m/schema [:string {:gen/fmap '(partial str "kikka_")}] {::m/disable-sci true})]
      (is (thrown-with-msg?
           #?(:clj Exception, :cljs js/Error)
           #":malli.core/sci-not-available"
           (mg/generator schema)))
      (is (thrown-with-msg?
           #?(:clj Exception, :cljs js/Error)
           #":malli.core/sci-not-available"
           (mg/generator [:string {:gen/fmap '(partial str "kikka_")}] {::m/disable-sci true})))
      (testing "direct options win"
        (is (mg/generator schema {::m/disable-sci false})))))

  (testing "generator override"
    (testing "without generator"
      (let [schema [:fn {:gen/fmap '(fn [_] (rand-int 10))}
                    '(fn [x] (<= 0 x 10))]
            generator (mg/generator schema)]
        (dotimes [_ 100]
          (m/validate schema (mg/generate generator)))))
    (testing "with generator"
      (is (re-matches #"kikka_\d+" (mg/generate [:and {:gen/fmap '(partial str "kikka_")} pos-int?])))))

  (testing "gen/elements"
    (is (every? #{1 2} (mg/sample [:and {:gen/elements [1 2]} int?] {:size 1000})))
    (is (every? #{"1" "2"} (mg/sample [:and {:gen/elements [1 2], :gen/fmap 'str} int?] {:size 1000}))))

  (testing "gen/schema"
    (is (every? #{1 2} (mg/sample [:int {:gen/schema [:int {:gen/elements [1 2]}]}] {:size 1000})))
    (is (every? #{"+1" "+2"} (mg/sample [:int {:gen/schema [:int {:gen/elements [1 2], :gen/fmap str}]
                                               :gen/fmap (partial str "+")}] {:size 1000}))))

  (testing "gen/gen"
    (is (every? #{1 2} (mg/sample [:and {:gen/gen (gen/elements [1 2])} int?] {:size 1000})))
    (is (every? #{"1" "2"} (mg/sample [:and {:gen/gen (gen/elements [1 2]) :gen/fmap str} int?] {:size 1000})))))

(defn- schema+coll-gen [type children-gen]
  (gen/let [children children-gen]
    (let [schema (into [type] children)]
      (gen/tuple (gen/return schema) (mg/generator schema)))))

(def ^:private seqex-child
  (let [s (gen/elements [string? int? keyword?])]
    (gen/one-of [s (gen/fmap #(vector :* %) s)])))

(defspec cat-test 100
  (for-all [[s coll] (schema+coll-gen :cat (gen/vector seqex-child))]
    (m/validate s coll)))

(defspec catn-test 100
  (for-all [[s coll] (->> (gen/vector (gen/tuple gen/keyword seqex-child))
                          (gen/such-that (fn [coll] (or (empty? coll) (apply distinct? (map first coll)))))
                          (schema+coll-gen :catn))]
    (m/validate s coll)))

(defspec alt-test 100
  (for-all [[s coll] (schema+coll-gen :alt (gen/not-empty (gen/vector seqex-child)))]
    (m/validate s coll)))

(defspec altn-test 100
  (for-all [[s coll] (->> (gen/not-empty (gen/vector (gen/tuple gen/keyword seqex-child)))
                          (gen/such-that (fn [coll] (or (empty? coll) (apply distinct? (map first coll)))))
                          (schema+coll-gen :altn))]
    (m/validate s coll)))

(defspec ?*+-test 100
  (for-all [[s coll] (gen/let [type (gen/elements [:? :* :+])
                               child seqex-child]
                       (let [schema [type child]]
                         (gen/tuple (gen/return schema) (mg/generator schema))))]
    (m/validate s coll)))

(defspec repeat-test 100
  (for-all [[s coll] (schema+coll-gen :repeat (gen/tuple
                                               (gen/let [min gen/nat
                                                         len gen/nat]
                                                 {:min min, :max (+ min len)})
                                               seqex-child))]
    (m/validate s coll)))

(deftest min-max-test

  (testing "valid properties"
    (are [schema]
      (is (every? #(<= 10 % 20) (map count (mg/sample schema {:size 1000}))))

      [:vector {:min 10, :max 20} int?]
      [:set {:min 10, :max 20} int?]
      [:string {:min 10, :max 20}]

      [:vector {:gen/min 10, :max 20} int?]
      [:set {:gen/min 10, :max 20} int?]
      [:string {:gen/min 10, :max 20}]

      [:vector {:min 10, :gen/max 20} int?]
      [:set {:min 10, :gen/max 20} int?]
      [:string {:gen/min 10, :max 20}]

      [:vector {:min 1, :gen/min 10, :max 100, :gen/max 20} int?]
      [:set {:min 1, :gen/min 10, :max 100, :gen/max 20} int?]
      [:string {:min 1, :gen/min 10, :max 100, :gen/max 20}]))

  (testing "invalid properties"
    (are [schema]
      (is (thrown? #?(:clj Exception, :cljs js/Error) (mg/sample schema {:size 1000})))

      ;; :gen/min less than :min
      [:vector {:min 11, :gen/min 10, :max 100, :gen/max 20} int?]
      [:set {:min 11, :gen/min 10, :max 100, :gen/max 20} int?]
      [:string {:min 11, :gen/min 10, :max 100, :gen/max 20}]

      ;; :gen/max over :max
      [:vector {:min 1, :gen/min 10, :max 100, :gen/max 200} int?]
      [:set {:min 1, :gen/min 10, :max 100, :gen/max 200} int?]
      [:string {:min 1, :gen/min 10, :max 100, :gen/max 200}])))

(deftest protocol-test
  (let [values #{1 2 3 5 8 13}
        schema (reify
                 m/Schema
                 (-parent [_] (reify m/IntoSchema (-type-properties [_])))
                 (-properties [_])
                 mg/Generator
                 (-generator [_ _] (gen/elements values)))]
    (is (every? values (mg/sample schema {:size 1000})))))

(deftest util-schemas-test
  (let [registry (merge (m/default-schemas) (mu/schemas))]
    (doseq [schema [[:merge {:title "merge"}
                     [:map [:x int?] [:y int?]]
                     [:map [:z int?]]]
                    [:union {:title "union"}
                     [:map [:x int?] [:y int?]]
                     [:map [:x string?]]]
                    [:select-keys {:title "select-keys"}
                     [:map [:x int?] [:y int?]]
                     [:x]]]
            :let [schema (m/schema schema {:registry registry})]]
      (is (every? (partial m/validate schema) (mg/sample schema {:size 1000}))))))

#?(:clj
   (deftest function-schema-test
     (let [=> (m/schema [:=> [:cat int? int?] int?])
           {:keys [input output]} (m/-function-info =>)]
       (is (every? #(m/validate output (apply % (mg/generate input))) (mg/sample => {:size 1000}))))

     (let [=> (m/schema [:function [:=> [:cat int?] int?] [:=> [:cat int? int?] int?]])]
       (is (every? #(m/validate int? (apply % (mg/generate [:or [:cat int?] [:cat int? int?]]))) (mg/sample => {:size 1000}))))))

(deftest recursive-schema-generation-test-307
  (let [sample (mg/generate [:schema {:registry {::A
                                                 [:cat
                                                  [:= ::a]
                                                  [:vector {:gen/min 2, :gen/max 2} [:ref ::A]]]}}
                             ::A] {:size 1, :seed 1})]
    (is (-> sample flatten count (> 1)))))

(deftest slow-recursive-test
  (let [schema [:schema {:registry {::A [:tuple [:= :A]]
                                    ::B [:tuple [:= :B]]
                                    ::C [:tuple [:= :C]]
                                    ::D [:tuple [:= :D]]
                                    ::E [:tuple [:= :E] [:ref ::item]]
                                    ::F [:tuple [:= :F] [:ref ::item]]
                                    ::G [:tuple [:= :G] [:ref ::item]]
                                    ::item [:multi {:dispatch first}
                                            [:A ::A]
                                            [:B ::B]
                                            [:C ::C]
                                            [:D ::D]
                                            [:E ::E]
                                            [:F ::F]
                                            [:G ::G]]}}
                ::E]
        valid? (m/validator schema)]
    (is (every? valid? (mg/sample schema {:size 10000})))))

(deftest recursive-distinct-col-test
  (is (not (every? empty? (mg/sample [:set
                                      {:registry {::foo :int}}
                                      [:ref ::foo]]
                                     {:size 1000})))))

(comment
  (gen/one-of
    [(gen/return nil)
     (gen/tuple (gen/return "ping")
                (gen/recursive-gen
                  (fn [pong])))
     ])

  [:schema
   {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
               ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
   ::ping]
  (->> (gen/sample
         (gen/recursive-gen
           (fn [ping]
             (gen/tuple (gen/return "ping")
                        (gen/recursive-gen
                          (fn [pong]
                            (gen/tuple (gen/return "pong")
                                       ping))
                          (gen/one-of
                            [(gen/return nil)
                             (gen/tuple (gen/return "pong")
                                        (gen/return nil))]))))
           (gen/one-of
             [(gen/return nil)
              (gen/tuple (gen/return "ping")
                         (gen/return nil))]))
         1000)
       (drop 75))

  [:schema
   {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
               ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
   ::ping]
  (->> (gen/sample
         (gen/recursive-gen
           (fn [ping]
             (gen/tuple (gen/return "ping")
                        (gen/recursive-gen
                          (fn [pong]
                            (gen/tuple (gen/return "pong")
                                       ping))
                          (gen/tuple (gen/return "pong")
                                     (gen/return nil)))))
           (gen/tuple (gen/return "ping")
                      (gen/return nil)))
         100)
       (drop 75))

  [:schema
   {:registry {::A [:tuple [:= "A"] [:maybe [:ref ::B]]]
               ::B [:tuple [:= "B"] [:maybe [:ref ::C]]]
               ::C [:tuple [:= "C"] [:maybe [:ref ::A]]]}}
   [:tuple ::A ::B]]
  (->> (gen/sample
         (gen/tuple
           (gen/recursive-gen
             (fn [A]
               (gen/tuple (gen/return "A")
                          (gen/recursive-gen
                            (fn [B]
                              (gen/tuple (gen/return "B")
                                         (gen/recursive-gen
                                           (fn [C]
                                             (gen/tuple (gen/return "C")
                                                        A))
                                           (gen/tuple (gen/return "C")
                                                      (gen/return nil)))))
                            (gen/tuple (gen/return "B")
                                       (gen/return nil)))))
             (gen/tuple (gen/return "A")
                        (gen/return nil)))
           (gen/recursive-gen
             (fn [B]
               (gen/tuple (gen/return "B")
                          (gen/recursive-gen
                            (fn [C]
                              (gen/tuple (gen/return "C")
                                         (gen/recursive-gen
                                           (fn [A]
                                             (gen/tuple (gen/return "A")
                                                        C))
                                           (gen/tuple (gen/return "A")
                                                      (gen/return nil)))))
                            (gen/tuple (gen/return "C")
                                       (gen/return nil)))))
             (gen/tuple (gen/return "B")
                        (gen/return nil)))
           )
         100)
       (drop 75))

  ;; linked list of ABC that never repeats
  [:schema
   {:registry {::A [:tuple [:= "A"] [:maybe [:or [:ref ::B] [:ref ::C]]]]
               ::B [:tuple [:= "B"] [:maybe [:or [:ref ::C] [:ref ::A]]]]
               ::C [:tuple [:= "C"] [:maybe [:or [:ref ::A] [:ref ::B]]]]}}
   [:tuple ::A]]
  (->> (gen/sample
         (gen/tuple
           (gen/recursive-gen
             (fn [A]
               (gen/tuple (gen/return "A")
                          (gen/one-of
                            [(gen/recursive-gen
                               (fn [B]
                                 (gen/tuple (gen/return "B")
                                            (gen/one-of
                                              [(gen/recursive-gen
                                                 (fn [C]
                                                   (gen/tuple (gen/return "C")
                                                              (gen/one-of
                                                                [A
                                                                 B])))
                                                 (gen/tuple (gen/return "C")
                                                            (gen/return nil)))
                                               A])))
                               (gen/tuple (gen/return "B")
                                          (gen/return nil)))
                             (gen/recursive-gen
                               (fn [C]
                                 (gen/tuple (gen/return "C")
                                            (gen/one-of
                                              [A
                                               (gen/recursive-gen
                                                 (fn [B]
                                                   (gen/tuple (gen/return "B")
                                                              (gen/one-of
                                                                [(gen/recursive-gen
                                                                   (fn [C]
                                                                     (gen/tuple (gen/return "C")
                                                                                (gen/one-of
                                                                                  [A
                                                                                   B])))
                                                                   (gen/tuple (gen/return "C")
                                                                              (gen/return nil)))
                                                                 A])))
                                                 (gen/tuple (gen/return "B")
                                                            (gen/return nil)))])))
                               (gen/tuple (gen/return "C")
                                          (gen/return nil)))])))
             (gen/tuple (gen/return "A")
                        (gen/return nil))))
         100)
       (drop 75))

  [:schema
   {:registry {::data    [:or
                          ::int
                          ::vector]
               ::int     :int
               ::vector  [:vector
                          [:ref ::data]]}}
   ::data]
  (->> (gen/sample
         (gen/recursive-gen
           (fn [data]
             (gen/vector
               data))
           gen/large-integer)
         100)
       (drop 75))

  [:schema
   {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
               ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
   [:tuple ::ping ::pong]]
  (->> (gen/sample
         (gen/tuple
           (gen/recursive-gen
             (fn [ping]
               (gen/tuple (gen/return "ping")
                          (gen/tuple (gen/return "pong")
                                     ping)))
             (gen/tuple (gen/return "ping")
                        (gen/return nil)))
           (gen/recursive-gen
             (fn [pong]
               (gen/tuple (gen/return "pong")
                          (gen/tuple (gen/return "ping")
                                     pong)))
             (gen/tuple (gen/return "pong")
                        (gen/return nil))))
         100)
       (drop 75))
    [:schema {:registry {::A [:tuple [:= :A]]
                         ::B [:tuple [:= :B]]
                         ::C [:tuple [:= :C]]
                         ::D [:tuple [:= :D]]
                         ::E [:tuple [:= :E] [:ref ::item]]
                         ::F [:tuple [:= :F] [:ref ::item]]
                         ::G [:tuple [:= :G] [:ref ::item]]
                         ::item [:multi {:dispatch first}
                                 [:A ::A]
                                 [:B ::B]
                                 [:C ::C]
                                 [:D ::D]
                                 [:E ::E]
                                 [:F ::F]
                                 [:G ::G]]}}
     ::E]
  (->> (gen/sample
         (gen/recursive-gen
           (fn [E]
             (gen/tuple (gen/return :E)
                        (gen/recursive-gen
                          (fn [item]
                            (gen/one-of
                              [E
                               (gen/recursive-gen
                                 (fn [F]
                                   (gen/tuple (gen/return :F)
                                              (gen/recursive-gen
                                                (fn [item]
                                                  (gen/one-of
                                                    [E
                                                     F
                                                     (gen/recursive-gen
                                                       (fn [G]
                                                         (gen/tuple (gen/return :G)
                                                                    (gen/recursive-gen
                                                                      (fn [item]
                                                                        (gen/one-of
                                                                          [E
                                                                           F
                                                                           G]))
                                                                      (gen/one-of
                                                                        (mapv #(gen/tuple (gen/return %))
                                                                              [:A :B :C :D])))))
                                                       (gen/tuple (gen/return :E)
                                                                  (gen/one-of
                                                                    (mapv #(gen/tuple (gen/return %))
                                                                          [:A :B :C :D]))))]))
                                                (gen/one-of
                                                  (mapv #(gen/tuple (gen/return %))
                                                        [:A :B :C :D])))))
                                 (gen/tuple (gen/return :E)
                                            (gen/one-of
                                              (mapv #(gen/tuple (gen/return %))
                                                    [:A :B :C :D]))))
                               (gen/recursive-gen
                                 (fn [G]
                                   (gen/tuple (gen/return :G)
                                              (gen/recursive-gen
                                                (fn [item]
                                                  (gen/one-of
                                                    [E
                                                     (gen/recursive-gen
                                                       (fn [F]
                                                         (gen/tuple (gen/return :F)
                                                                    (gen/recursive-gen
                                                                      (fn [item]
                                                                        (gen/one-of
                                                                          [E
                                                                           F
                                                                           G]))
                                                                      (gen/one-of
                                                                        (mapv #(gen/tuple (gen/return %))
                                                                              [:A :B :C :D])))))
                                                       (gen/tuple (gen/return :E)
                                                                  (gen/one-of
                                                                    (mapv #(gen/tuple (gen/return %))
                                                                          [:A :B :C :D]))))
                                                     G]))
                                                (gen/one-of
                                                  (mapv #(gen/tuple (gen/return %))
                                                        [:A :B :C :D])))))
                                 (gen/tuple (gen/return :E)
                                            (gen/one-of
                                              (mapv #(gen/tuple (gen/return %))
                                                    [:A :B :C :D]))))]))
                          (gen/one-of
                            (mapv #(gen/tuple (gen/return %))
                                        [:A :B :C :D])))))
           (gen/tuple (gen/return :E)
                      (gen/one-of
                        (mapv #(gen/tuple (gen/return %))
                              [:A :B :C :D]))))
         100)
       (drop 75))
  )

#_
(let [id (gensym)]
 (with-meta
   [:schema
    {:registry {::ping [:maybe [:tuple [:= "ping"]
                                (with-meta [:ref ::pong]
                                           {::registry-id id})]]
                ::pong [:maybe [:tuple [:= "pong"]
                                (with-meta [:ref ::ping]
                                           {::registry-id id})]]}}
    ::ping]
   {::registry-id id}))

(deftest scalar-container-schema-test
  (let [test-cases [{:schema [:schema
                              {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                                          ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                              ::ping]
                     :generator 
                     (gen/recursive-gen
                       (fn [ping]
                         ;; container
                         (gen/tuple (gen/return "ping")
                                    ;; Note: scalar gen has nilable case
                                    (gen/tuple (gen/return "pong")
                                               ping)))
                       ;; scalar
                       (gen/one-of
                         [(gen/return nil)
                          (gen/tuple (gen/return "ping")
                                     (gen/return nil))]))
                     :scalar-schema [:maybe [:tuple [:= "ping"] :nil]]
                     :container-schema 
                     #_ ;;FIXME
                     [:tuple [:= "ping"]
                      [:tuple [:= "pong"]
                       [:ref ::ping]]]
                     [:maybe [:tuple [:= "ping"] [:maybe [:tuple [:= "pong"] [:ref ::ping]]]]]}
                    {:schema [:schema
                              {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
                                          ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
                              ::ping]
                     :generator
                     (gen/recursive-gen
                       (fn [ping]
                         ;; container
                         (gen/tuple (gen/return "ping")
                                    (gen/tuple (gen/return "pong")
                                               ping)))
                       ;; scalar
                       (gen/tuple (gen/return "ping")
                                  (gen/one-of
                                    [(gen/return nil)
                                     (gen/tuple (gen/return "pong")
                                                (gen/return nil))])))
                     :scalar-schema [:tuple [:= "ping"] [:maybe [:tuple [:= "pong"] :nil]]]
                     :container-schema 
                     ;;FIXME
                     #_[:tuple [:= "ping"]
                        [:tuple [:= "pong"]
                         [:ref ::ping]]]
                     [:tuple [:= "ping"] [:maybe [:tuple [:= "pong"] [:maybe [:ref ::ping]]]]]}
                    {:schema [:schema
                              {:registry {::data    [:or
                                                     ::int
                                                     ::vector]
                                          ::int     :int
                                          ::vector  [:vector
                                                     [:ref ::data]]}}
                              ::data]
                     :generator
                     (gen/recursive-gen
                       (fn [data]
                         (gen/vector
                           data))
                       gen/large-integer)
                     :scalar-schema [:or :int [:vector {:max 0} :any]]
                     :container-schema 
                     ;;FIXME
                     #_[:vector [:ref ::data]]
                     [:or :int [:vector [:ref ::data]]]}

                    {:schema [:schema {:registry {::A
                                                  [:cat
                                                   [:= ::a]
                                                   [:vector {:gen/min 2, :gen/max 2} [:ref ::A]]]}}
                              ::A]
                     :generator (gen/recursive-gen
                                  (fn [A]
                                    (->> (gen/tuple (gen/return ::a)
                                                    (gen/vector A 2 2))
                                         (gen/fmap list*)))
                                  (->> (gen/tuple (gen/return ::a)
                                                  ;; [:vector :never]
                                                  (gen/tuple))
                                       (gen/fmap list*)))
                     :scalar-schema [:cat [:= ::a] [:vector {:max 0} :any]]
                     :container-schema 
                     [:cat [:= ::a] [:vector {:gen/min 2 :gen/max 2} [:ref ::A]]]}
                   {:schema [:schema {:registry {::rec [:maybe [:ref ::rec]]}} ::rec]
                    :generator (gen/recursive-gen
                                 (fn [rec] rec)
                                 (gen/return nil))
                    :scalar-schema :nil
                    :container-schema 
                    ;;FIXME
                    #_[:ref ::rec]
                    [:maybe [:ref ::rec]]}
                   {:schema [:schema {:registry {::rec [:map [:rec {:optional true} [:ref ::rec]]]}} ::rec]
                    :generator (gen/recursive-gen
                                 (fn [rec]
                                   (gen/fmap (fn [rec]
                                               {:rec rec})
                                             rec))
                                 (gen/return {}))
                    :scalar-schema [:map {:closed true}]
                    :container-schema 
                    #_ ;;FIXME
                    [:map [:rec [:ref ::rec]]]
                    [:map [:rec {:optional true} [:ref ::rec]]]}
                  {:schema [:schema {:registry {::tuple [:tuple boolean? [:ref ::or]]
                                                ::or [:or int? ::tuple]}} ::or]
                   :generator (gen/recursive-gen
                                (fn [OR]
                                  (gen/tuple gen/boolean
                                             OR))
                                gen/large-integer)
                   :scalar-schema 'int?
                   :container-schema 
                   ;;FIXME
                   #_'[:tuple boolean? [:ref ::or]]
                   '[:or int? [:tuple boolean? [:ref ::or]]]}
                 {:schema [:schema {:registry {::rec [:tuple int? [:vector {:max 2} [:ref ::rec]]]}} ::rec]
                    #_(comment
                        (->> (gen/sample
                               (gen/recursive-gen
                                 (fn [rec]
                                   (gen/vector rec 0 2))
                                 (gen/tuple gen/large-integer (gen/tuple)))
                               100)
                             (drop 75)))
                  :scalar-schema [:tuple 'int? [:vector {:max 0} :any]]
                  :container-schema 
                  ;;FIXME
                  #_[:vector {:max 2} [:ref ::rec]]
                  [:tuple 'int? [:vector {:max 2} [:ref ::rec]]]}
                {:schema [:schema {:registry {::rec [:tuple int? [:set {:max 2} [:ref ::rec]]]}} ::rec]
                 :generator (gen/recursive-gen
                              (fn [rec]
                                (gen/fmap set (gen/vector-distinct rec {:max-elements 2 :max-tries 100})))
                              (gen/tuple gen/large-integer (gen/return #{})))
                 :scalar-schema [:tuple 'int? [:set {:max 0} :any]]
                 :container-schema
                 [:tuple 'int? [:set {:max 2} [:ref ::rec]]]}
                {:schema [:schema {:registry {::multi
                                              [:multi {:dispatch :type}
                                               [:int [:map [:type [:= :int]] [:int int?]]]
                                               [:multi [:map [:type [:= :multi]] [:multi {:optional true} [:ref ::multi]]]]]}}
                          ::multi]
                    #_(comment
                        ;;TODO
                        (->> (gen/sample
                               (gen/recursive-gen
                                 (fn [multi]
                                   )
                                 ()
                                 )
                               100)
                             (drop 75)))
                 :scalar-schema [:multi {:dispatch :type}
                                 [:int [:map [:type [:= :int]] [:int 'int?]]]
                                 [:multi [:map {:closed true} [:type [:= :multi]]]]]
                 :container-schema 
                 #_ ;;FIXME
                 [:multi
                  {:dispatch :type}
                  [:multi [:map [:type [:= :multi]] [:multi [:ref :malli.generator-test/multi]]]]]
                 '[:multi
                   {:dispatch :type}
                   [:int [:map [:type [:= :int]] [:int int?]]]
                   [:multi [:map [:type [:= :multi]] [:multi {:optional true} [:ref :malli.generator-test/multi]]]]]
}
]]
    (doseq [{:keys [schema scalar-schema container-schema]} test-cases]
      (testing schema
        (is (= scalar-schema (m/form
                               (mg/schema->scalar-schema schema
                                                         {}))))
        (is (= container-schema (m/form
                                  (mg/schema->container-schema schema
                                                               {}))))))))

(deftest schema->scalar-schema-test
  (is (= [:map {:closed true}]
         (m/form
           (mg/schema->scalar-schema
             [:schema {:registry {::rec [:map [:rec {:optional true} [:ref ::rec]]]}} ::rec]
             {})))))

(deftest schema->container-schema-test
  (is (= [:maybe
          [:tuple
           [:= "ping"]
           [:maybe
            [:tuple [:= "pong"] [:ref :malli.generator-test/ping]]]]]
         (m/form
           (mg/schema->container-schema
             [:schema
              {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                          ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
              ::ping]
             {}))))
  ;;ensure recursive free variables are preserved so recursive generators can be added
  (is (= [:maybe
          [:tuple
           [:= "ping"]
           [:maybe
            [:tuple
             [:= "pong"]
             [:ref :malli.generator-test/flub]
             [:ref :malli.generator-test/ping]]]]]
         (m/form
           (mg/schema->container-schema
             (m/deref
               [:schema
                {:registry {::flub :int}}
                [:schema
                 {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                             ::pong [:maybe [:tuple [:= "pong"] [:ref ::flub] [:ref ::ping]]]}}
                 ::ping]])
             {::mg/rec-gen {::flub (gen/return nil)}}))))
  (is (= [:map [:rec {:optional true} [:ref ::rec]]]
         (m/form
           (mg/schema->container-schema
             [:schema {:registry
                       {::rec [:map 
                               [:rec {:optional true} [:ref ::rec]]]}}
              ::rec]
             {})))))
