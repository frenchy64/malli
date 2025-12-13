(ns malli.doc
  (:require [malli.global :as glo]
            [malli.core :as m]
            [clojure.java.io :as io]
            [edamame.core :as e :refer [parse-string]]))

(def ^:private global-schemas-meta (atom {}))

(defn -register-schema-meta!
  ([k m] (run! #(-register-schema-meta! k m %) [:clj :cljs]))
  ([k m platform] (swap! global-schemas-meta assoc-in [platform k] m)))

(defn -schema-meta [k platform]
  (-> @global-schemas-meta (get platform) (get k)))

(defn -doc [qkw platform]
  (-schema-meta qkw platform))

(defn -collect-meta [defining-ns-regex platform]
  (into (sorted-map)
        (map (fn [[k {:keys [ns] :as m}]]
               (when (and ns (re-matches defining-ns-regex (name ns)))
                 [k m])))
        (get @global-schemas-meta platform)))

;; modified from clojure.repl
(defn -source-fn
  [{:keys [file ns line column]}]
  (when ns
    (when file
      (when-some [strm (or (io/resource file)
                           (let [f (io/file file)]
                             (when (.exists f)
                               (-> f .toURI .toURL))))]
        (with-open [rdr (java.io.LineNumberReader. (io/reader strm))]
          ;;TODO navigate to column
          (dotimes [_ (dec line)] (.readLine rdr))
          (with-open [pbr (e/source-reader rdr)]
            (second (e/parse-next+string pbr {:read-eval (fn [_] nil)
                                              :read-cond :preserve
                                              :auto-resolve (assoc (ns-aliases ns) :current ns)}))))))))

(defn print-doc [reference platform]
  (when-some [{:keys [into-schema doc] :as m} (-schema-meta reference platform)]
    (println "-------------------------")
    (cond
      into-schema (do (println "Schema constructor")
                      (println (pr-str m)))
      :else (do (println "Named Schema")
                (when doc
                  (println)
                  (println (:doc m)))
                (println)
                (when-some [form (when (= :clj platform)
                                   (get @@#'glo/global-schemas reference))]
                  (println form)
                  (println (:schema-form m)))
                (let [source (-source-fn m)]
                  (when source
                    (println)
                    (println "Source code:")
                    (println source)))))))

(defn save! [file defining-ns-regex platform]
  (spit file (pr-str (-collect-meta defining-ns-regex platform))))

(defmacro doc [qkw]
  (print-doc qkw (if (:ns &env) :cljs :clj)))
