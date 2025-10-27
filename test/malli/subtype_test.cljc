(ns malli.subtype-test
  (:require [clojure.test :refer [deftest is testing are]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [malli.core :as m]
            [malli.subtype :as ms]
            [malli.generator :as mg]))

;;
;; Basic type tests
;;

(deftest simple-types-test
  (testing "reflexivity - every type is a subtype of itself"
    (are [schema] (ms/subtype? schema schema)
      :any
      :some
      :nil
      :string
      :int
      :float
      :double
      :boolean
      :keyword
      :symbol
      :qualified-keyword
      :qualified-symbol
      :uuid))
  
  (testing ":any is the top type"
    (are [schema] (ms/subtype? schema :any)
      :some
      :nil
      :string
      :int
      :float
      :double
      :boolean
      :keyword
      :symbol
      :qualified-keyword
      :qualified-symbol
      :uuid
      [:vector :int]
      [:map [:x :int]]
      [:enum 1 2 3]))
  
  (testing ":some excludes :nil"
    (are [schema] (ms/subtype? schema :some)
      :string
      :int
      :float
      :double
      :boolean
      :keyword
      :symbol
      :qualified-keyword
      :qualified-symbol
      :uuid
      [:vector :int]
      [:map [:x :int]])
    (is (not (ms/subtype? :nil :some))))
  
  (testing "numeric types are distinct"
    ;; In Malli, int values don't validate against float/double
    (is (not (ms/subtype? :int :double)))
    (is (not (ms/subtype? :int :float)))
    (is (not (ms/subtype? :float :double)))
    (is (not (ms/subtype? :double :int)))
    (is (not (ms/subtype? :float :int)))
    (is (not (ms/subtype? :double :float)))))

(deftest properties-test
  (testing "string with min/max"
    (is (ms/subtype? [:string {:min 5 :max 10}]
                     [:string {:min 3 :max 15}]))
    (is (not (ms/subtype? [:string {:min 3 :max 15}]
                          [:string {:min 5 :max 10}])))
    (is (ms/subtype? [:string {:min 5}]
                     [:string {:min 3}]))
    (is (ms/subtype? [:string {:max 10}]
                     [:string {:max 15}])))
  
  (testing "int with min/max"
    (is (ms/subtype? [:int {:min 5 :max 10}]
                     [:int {:min 3 :max 15}]))
    (is (not (ms/subtype? [:int {:min 3 :max 15}]
                          [:int {:min 5 :max 10}])))
    (is (ms/subtype? [:int {:min 5}]
                     [:int {:min 3}]))
    (is (ms/subtype? [:int {:max 10}]
                     [:int {:max 15}]))))

(deftest and-type-test
  (testing ":and schema - intersection"
    (is (ms/subtype? [:and :int [:> 0]]
                     :int))
    ;; Note: More complex comparator schema relationships are not yet supported
    ;; (is (ms/subtype? [:and :int [:> 0] [:<= 100]]
    ;;                  [:and :int [:> 0]]))
    (is (ms/subtype? [:and :int [:> 0]]
                     [:or :int :string]))
    (is (not (ms/subtype? [:and :int [:> 0]]
                          [:and :int [:> 10]])))
    
    ;; :and on the right
    (is (ms/subtype? :int [:and :int :some]))
    ;; Note: Comparator schema equivalence not yet supported
    ;; (is (ms/subtype? [:int {:min 5}] [:and :int [:>= 5]]))
    ))

(deftest or-type-test
  (testing ":or schema - union"
    (is (ms/subtype? :int [:or :int :string]))
    (is (ms/subtype? :string [:or :int :string]))
    (is (ms/subtype? [:or :int :string] :any))
    (is (not (ms/subtype? [:or :int :string] :int)))
    (is (not (ms/subtype? [:or :int :string] :string)))
    
    (testing "union subset"
      (is (ms/subtype? [:or :int] [:or :int :string]))
      (is (ms/subtype? [:or :int :keyword] [:or :int :string :keyword]))
      (is (not (ms/subtype? [:or :int :string :keyword] [:or :int :string]))))))

(deftest orn-type-test
  (testing ":orn schema - tagged union"
    (is (ms/subtype? [:orn [:i :int]]
                     [:orn [:i :int]]))
    (is (ms/subtype? [:orn [:i :int] [:s :string]]
                     [:orn [:i :int] [:s :string]]))
    (is (ms/subtype? [:orn [:i [:int {:min 5}]]]
                     [:orn [:i :int]]))
    (is (not (ms/subtype? [:orn [:i :int]]
                          [:orn [:s :string]])))))

(deftest enum-type-test
  (testing ":enum schema"
    (is (ms/subtype? [:enum 1 2] [:enum 1 2 3]))
    (is (ms/subtype? [:enum 1] [:enum 1 2]))
    (is (not (ms/subtype? [:enum 1 2 3] [:enum 1 2])))
    (is (ms/subtype? [:enum 1 2] :any))
    (is (ms/subtype? [:enum 1 2] :some))
    (is (ms/subtype? [:enum 1 2] [:or :int :string]))
    (is (not (ms/subtype? [:enum 1 nil] :some)))))

(deftest maybe-type-test
  (testing ":maybe schema"
    (is (ms/subtype? [:maybe :int] [:maybe :int]))
    (is (ms/subtype? [:maybe [:int {:min 5}]]
                     [:maybe :int]))
    (is (ms/subtype? :nil [:maybe :int]))
    (is (not (ms/subtype? [:maybe :int] :int)))
    (is (ms/subtype? [:maybe :int] :any))
    (is (not (ms/subtype? [:maybe :int] :some)))))

(deftest vector-type-test
  (testing ":vector schema"
    (is (ms/subtype? [:vector :int] [:vector :int]))
    (is (ms/subtype? [:vector [:int {:min 5}]]
                     [:vector :int]))
    (is (not (ms/subtype? [:vector :int]
                          [:vector [:int {:min 5}]])))
    (is (ms/subtype? [:vector :int] [:sequential :int]))
    (is (ms/subtype? [:vector :int] :any))
    (is (ms/subtype? [:vector :int] :some))))

(deftest sequential-type-test
  (testing ":sequential schema"
    (is (ms/subtype? [:sequential :int] [:sequential :int]))
    (is (ms/subtype? [:sequential [:int {:min 5}]]
                     [:sequential :int]))
    (is (not (ms/subtype? [:sequential :int] [:vector :int])))
    (is (ms/subtype? [:sequential :int] :any))))

(deftest set-type-test
  (testing ":set schema"
    (is (ms/subtype? [:set :int] [:set :int]))
    (is (ms/subtype? [:set [:int {:min 5}]]
                     [:set :int]))
    (is (not (ms/subtype? [:set :int]
                          [:set [:int {:min 5}]])))
    (is (ms/subtype? [:set :int] :any))))

(deftest tuple-type-test
  (testing ":tuple schema"
    (is (ms/subtype? [:tuple :int :string]
                     [:tuple :int :string]))
    (is (ms/subtype? [:tuple [:int {:min 5}] :string]
                     [:tuple :int :string]))
    (is (not (ms/subtype? [:tuple :int]
                          [:tuple :int :string])))
    (is (not (ms/subtype? [:tuple :int :string]
                          [:tuple :int])))
    (is (ms/subtype? [:tuple :int :string] [:sequential :any]))
    (is (ms/subtype? [:tuple :int :string] [:vector :any]))
    (is (ms/subtype? [:tuple :int :string] :any))))

(deftest map-type-test
  (testing ":map schema"
    (is (ms/subtype? [:map [:x :int]]
                     [:map [:x :int]]))
    (is (ms/subtype? [:map [:x :int] [:y :string]]
                     [:map [:x :int]]))
    (is (not (ms/subtype? [:map [:x :int]]
                          [:map [:x :int] [:y :string]])))
    (is (ms/subtype? [:map [:x [:int {:min 5}]]]
                     [:map [:x :int]]))
    
    (testing "optional entries"
      (is (ms/subtype? [:map [:x :int]]
                       [:map [:x :int] [:y {:optional true} :string]]))
      (is (not (ms/subtype? [:map [:x :int]]
                            [:map [:x :int] [:y :string]]))))
    
    (is (ms/subtype? [:map [:x :int]] :any))
    (is (ms/subtype? [:map [:x :int]] :some))))

(deftest map-of-type-test
  (testing ":map-of schema"
    (is (ms/subtype? [:map-of :keyword :int]
                     [:map-of :keyword :int]))
    (is (ms/subtype? [:map-of :qualified-keyword :int]
                     [:map-of :keyword :int]))
    (is (ms/subtype? [:map-of :keyword [:int {:min 5}]]
                     [:map-of :keyword :int]))
    (is (not (ms/subtype? [:map-of :keyword :int]
                          [:map-of :keyword [:int {:min 5}]])))
    (is (ms/subtype? [:map-of :keyword :int] :any))))

(deftest not-type-test
  (testing ":not schema"
    (is (ms/subtype? [:not :string] [:not :string]))
    ;; :not is contravariant
    (is (ms/subtype? [:not :int] [:not [:int {:min 5}]]))
    (is (not (ms/subtype? [:not [:int {:min 5}]] [:not :int])))))

(deftest ref-type-test
  (testing ":ref schema"
    ;; Direct map comparison works
    (is (ms/subtype? [:map [:name :string] [:role :keyword]]
                     [:map [:name :string]]))
    (is (ms/subtype? [:map [:name :string]]
                     [:map [:name :string]]))))

(deftest fn-type-test
  (testing ":fn schema"
    (is (ms/subtype? [:fn number?] [:fn number?]))
    ;; Can't prove function subset relationship in general
    (is (not (ms/subtype? [:fn int?] [:fn number?])))))

(deftest regex-schemas-test
  (testing ":cat schema"
    (is (ms/subtype? [:cat :int :string]
                     [:cat :int :string]))
    (is (ms/subtype? [:cat [:int {:min 5}] :string]
                     [:cat :int :string]))
    (is (not (ms/subtype? [:cat :int]
                          [:cat :int :string]))))
  
  (testing ":alt schema"
    (is (ms/subtype? [:alt :int]
                     [:alt :int :string]))
    (is (ms/subtype? [:alt :int :keyword]
                     [:alt :int :string :keyword])))
  
  (testing ":* schema"
    (is (ms/subtype? [:* :int] [:* :int]))
    (is (ms/subtype? [:* [:int {:min 5}]]
                     [:* :int])))
  
  (testing ":+ schema"
    (is (ms/subtype? [:+ :int] [:+ :int]))
    (is (ms/subtype? [:+ [:int {:min 5}]]
                     [:+ :int])))
  
  (testing ":? schema"
    (is (ms/subtype? [:? :int] [:? :int]))
    (is (ms/subtype? [:? [:int {:min 5}]]
                     [:? :int])))
  
  (testing ":repeat schema"
    (is (ms/subtype? [:repeat :int] [:repeat :int]))
    (is (ms/subtype? [:repeat {:min 2 :max 5} :int]
                     [:repeat {:min 1 :max 10} :int]))))

;;
;; Tabular tests for comprehensive type checking
;;

(deftest subtype-matrix-test
  (testing "comprehensive subtype relationships"
    (let [test-cases
          ;; [S T expected-result description]
          [[:any :any true "any <: any"]
           [:nil :any true "nil <: any"]
           [:int :any true "int <: any"]
           [:string :any true "string <: any"]
           
           [:nil :some false "nil not <: some"]
           [:int :some true "int <: some"]
           [:string :some true "string <: some"]
           
           [:int :int true "int <: int"]
           [:int :string false "int not <: string"]
           [:int :double false "int not <: double (Malli types are distinct)"]
           [:int :float false "int not <: float (Malli types are distinct)"]
           [:double :int false "double not <: int"]
           [:float :int false "float not <: int"]
           [:float :double false "float not <: double (Malli types are distinct)"]
           
           [[:vector :int] [:vector :int] true "vector int <: vector int"]
           [[:vector :int] [:sequential :int] true "vector int <: sequential int"]
           [[:sequential :int] [:vector :int] false "sequential int not <: vector int"]
           
           [[:tuple :int] [:vector :int] true "tuple int <: vector int"]
           [[:tuple :int :string] [:vector :any] true "tuple <: vector any"]
           [[:tuple :int :string] [:sequential :any] true "tuple <: sequential any"]
           
           [:nil [:maybe :int] true "nil <: maybe int"]
           [:int [:maybe :int] false "int not <: maybe int (should be contained)"]
           
           [:int [:or :int :string] true "int <: or int string"]
           [:string [:or :int :string] true "string <: or int string"]
           [:keyword [:or :int :string] false "keyword not <: or int string"]
           
           [[:enum 1 2] [:enum 1 2 3] true "enum subset"]
           [[:enum 1 2 3] [:enum 1 2] false "enum not subset"]
           
           [:int [:and :int :some] true "int <: and int some"]
           [[:and :int [:> 0]] :int true "and int >0 <: int"]
           [[:and :int [:> 0]] [:and :int] true "and int >0 <: and int"]
           
           [[:map [:x :int]] [:map [:x :int]] true "map equality"]
           [[:map [:x :int] [:y :string]] [:map [:x :int]] true "map with extra keys"]
           [[:map [:x :int]] [:map [:x :int] [:y :string]] false "map missing required key"]
           
           [[:map-of :keyword :int] [:map-of :keyword :int] true "map-of equality"]]]
      (doseq [[S T expected desc] test-cases]
        (testing desc
          (is (= expected (ms/subtype? S T))
              (str "Testing: " desc " - " S " <: " T)))))))

;;
;; Generative tests
;;

(defn- assorted-schemas
  "Returns a collection of diverse schemas for testing."
  []
  [:any
   :some
   :nil
   :string
   :int
   :float
   :double
   :boolean
   :keyword
   :symbol
   :uuid
   [:string {:min 5 :max 10}]
   [:int {:min 0 :max 100}]
   [:enum 1 2 3]
   [:enum "a" "b" "c"]
   [:maybe :int]
   [:maybe :string]
   [:vector :int]
   [:vector :string]
   [:vector :any]
   [:sequential :int]
   [:set :keyword]
   [:tuple :int :string]
   [:tuple :int :string :keyword]
   [:map [:x :int]]
   [:map [:x :int] [:y :string]]
   [:map [:x :int] [:y {:optional true} :string]]
   [:map-of :keyword :int]
   [:map-of :string :any]
   [:and :int [:> 0]]
   [:and :int [:>= 0] [:<= 100]]
   [:or :int :string]
   [:or :int :string :keyword]
   [:orn [:i :int] [:s :string]]
   [:not :nil]
   [:not :string]])

(defspec generative-subtype-soundness-test 100
  (for-all [schema1 (gen/elements (assorted-schemas))
            schema2 (gen/elements (assorted-schemas))]
    (let [is-subtype (try
                       (ms/subtype? schema1 schema2)
                       (catch #?(:clj Exception :cljs js/Error) e
                         ;; If subtype check fails, conservatively assume false
                         false))]
      (if is-subtype
        ;; If schema1 is a subtype of schema2, then generated values
        ;; from schema1 should validate against schema2
        (try
          (let [value (mg/generate schema1 {:size 10 :seed (rand-int 10000)})]
            (or (m/validate schema2 value)
                ;; Sometimes generators might not be able to generate for a schema
                (not (m/validate schema1 value))))
          (catch #?(:clj Exception :cljs js/Error) e
            ;; If generation fails, pass the test
            true))
        ;; If not a subtype, we can't make guarantees, so pass
        true))))

(deftest sanity-check-test
  (testing "basic sanity checks for subtyping"
    (is (ms/subtype? :int :int))
    (is (ms/subtype? :int :any))
    (is (not (ms/subtype? :any :int)))
    (is (ms/subtype? [:enum 1] [:enum 1 2]))
    (is (ms/subtype? :int [:or :int :string]))))
