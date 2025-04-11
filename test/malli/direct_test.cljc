(ns malli.direct-test
  (:require [malli.direct :as md]
            [malli.core :as m]
            [clojure.test :refer [is deftest]]))

(set! *print-namespace-maps* false)

#?(:clj
   (defn ->dgensym []
     (let [counter (atom -1)]
       (fn
         ([] (symbol (str "G__" (swap! counter inc))))
         ([n] (symbol (str (name n) (swap! counter inc))))))))

#?(:clj
   (defmacro with-deterministic-gensym
     [& body]
     `(let [gensym# (->dgensym)]
        (with-bindings {#'md/*gensym* gensym#}
          (do ~@body)))))

#?(:clj (defn ddirect* [s] (with-deterministic-gensym (md/direct* s))))

(deftest direct-atomic-test
  #?(:clj (is (= '(clojure.core/let
                    [o0 {:registry (malli.core/-registry)}]
                    (malli.core/-into-schema
                      (malli.registry/schema (:registry o0) ':int)
                      'nil
                      []
                      o0))
                 (ddirect* :int))))
  (is (m/schema? (md/direct :int)))
  (is (= :int (m/form (md/direct :int)))))

(deftest direct-nested-test
  #?(:clj (is (= '(clojure.core/let
                    [o0 {:registry (malli.core/-registry)}]
                    (malli.core/-into-schema
                      (malli.registry/schema (:registry o0) ':vector)
                      'nil
                      [(malli.core/-into-schema
                         (malli.registry/schema (:registry o0) ':int)
                         'nil
                         []
                         o0)]
                      o0))
                 (ddirect* [:vector :int]))))
  (is (m/schema? (md/direct [:vector :int])))
  (is (= [:vector :int] (m/form (md/direct [:vector :int])))))

(deftest direct-registry-test
  #?(:clj (is (= '(clojure.core/let
                    [o0 {:registry (malli.core/-registry)}]
                    (clojure.core/let
                      [r1
                       '#:malli.direct-test{:foo :int}
                       o0
                       (malli.core/-update
                         o0
                         :registry
                         (clojure.core/fn
                           [x2]
                           (malli.registry/composite-registry
                             r1
                             (clojure.core/or x2 (malli.core/-registry o0)))))
                       p3
                       {:registry
                        {':malli.direct-test/foo
                         (malli.core/-into-schema
                           (malli.registry/schema (:registry o0) ':int)
                           'nil
                           []
                           o0)}}]
                      (malli.core/-into-schema
                        (malli.registry/schema (:registry o0) ':int)
                        p3
                        []
                        o0)))
                 (ddirect* [:int {:registry {::foo :int}}]))))
  (is (m/schema? (md/direct [:int {:registry {::foo :int}}])))
  (is (= [:int {:registry {::foo :int}}]
         (m/form (md/direct [:int {:registry {::foo :int}}])))))

(deftest direct-nested-registry-test
  #?(:clj (is (= '(clojure.core/let
                    [o0 {:registry (malli.core/-registry)}]
                    (clojure.core/let
                      [r1
                       '#:malli.direct-test{:foo :int}
                       o0
                       (malli.core/-update
                         o0
                         :registry
                         (clojure.core/fn
                           [x2]
                           (malli.registry/composite-registry
                             r1
                             (clojure.core/or x2 (malli.core/-registry o0)))))
                       p3
                       {:registry
                        {':malli.direct-test/foo
                         (malli.core/-into-schema
                           (malli.registry/schema (:registry o0) ':int)
                           'nil
                           []
                           o0)}}]
                      (malli.core/-into-schema
                        (malli.registry/schema (:registry o0) ':vector)
                        p3
                        [(clojure.core/let
                           [r7
                            '#:malli.direct-test{:bar :malli.direct-test/foo}
                            o0
                            (malli.core/-update
                              o0
                              :registry
                              (clojure.core/fn
                                [x8]
                                (malli.registry/composite-registry
                                  r7
                                  (clojure.core/or x8 (malli.core/-registry o0)))))
                            p9
                            {:registry
                             {':malli.direct-test/bar
                              (malli.core/schema ':malli.direct-test/foo o0)}}]
                           (malli.core/-into-schema
                             (malli.registry/schema (:registry o0) ':vector)
                             p9
                             [(malli.core/schema ':malli.direct-test/foo o0)]
                             o0))]
                        o0)))
                 (ddirect* [:vector {:registry {::foo :int}} [:vector {:registry {::bar ::foo}} ::foo]]))))
  (is (m/schema? (md/direct [:vector {:registry {::foo :int}} [:vector {:registry {::bar ::foo}} ::foo]])))
  (is (= [:vector {:registry {::foo :int}} [:vector {:registry {::bar ::foo}} ::foo]]
         (m/form (md/direct [:vector {:registry {::foo :int}} [:vector {:registry {::bar ::foo}} ::foo]])))))

(deftest direct-ref-test
  #?(:clj (is (= '(clojure.core/let
                    [o0 {:registry (malli.core/-registry)}]
                    (clojure.core/let
                      [r1
                       '{:malli.direct-test/foo [:vector [:ref :malli.direct-test/foo]]}
                       o0
                       (malli.core/-update
                         o0
                         :registry
                         (clojure.core/fn
                           [x2]
                           (malli.registry/composite-registry
                             r1
                             (clojure.core/or x2 (malli.core/-registry o0)))))
                       p3
                       {:registry
                        {':malli.direct-test/foo
                         (malli.core/-into-schema
                           (malli.registry/schema (:registry o0) ':vector)
                           'nil
                           [(malli.core/-into-schema
                              (malli.registry/schema (:registry o0) ':ref)
                              'nil
                              '[:malli.direct-test/foo]
                              o0)]
                           o0)}}]
                      (malli.core/-into-schema
                        (malli.registry/schema (:registry o0) ':schema)
                        p3
                        [(malli.core/schema ':malli.direct-test/foo o0)]
                        o0)))
                 (ddirect* [:schema {:registry {::foo [:vector [:ref ::foo]]}} ::foo]))))
  (is (= [:schema {:registry {::foo [:vector [:ref ::foo]]}} ::foo]
         (m/form (md/direct [:schema {:registry {::foo [:vector [:ref ::foo]]}} ::foo])))))

(def foo :int)

(deftest direct-misc-test
  (is (= (md/direct #'foo))))

(def impl-dependent #?(:cljs :int :default :boolean))

#?(:cljs
   (deftest detect-out-of-sync-test
     (is (thrown? Exception (md/direct impl-dependent)))))
