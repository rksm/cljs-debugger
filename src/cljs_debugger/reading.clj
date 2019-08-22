(ns cljs-debugger.reading
  (:require [cljs-debugger.ast :as ast]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]
            [clojure.zip :as z])
  (:import java.lang.Math))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; FIXME: stolen frome cider
(defn merge-meta
  "Non-throwing version of (vary-meta obj merge metamap-1 metamap-2 ...).
  Like `vary-meta`, this only applies to immutable objects. For
  instance, this function does nothing on atoms, because the metadata
  of an `atom` is part of the atom itself and can only be changed
  destructively."
  {:style/indent 1}
  [obj & metamaps]
  (try
    (apply vary-meta obj merge metamaps)
    (catch Exception _e obj)))

(defn- walk-indexed
  "Walk through form calling (f coor element).
  The value of coor is a vector of indices representing element's
  address in the form. Unlike `clojure.walk/walk`, all metadata of
  objects in the form is preserved."
  ([f form] (walk-indexed [] f form))
  ([coor f form]
   (let [map-inner (fn [forms]
                     (map-indexed #(walk-indexed (conj coor %1) f %2)
                                  forms))
         ;; Maps are unordered, but we can try to use the keys (and
         ;; they're important enough that we're willing to risk
         ;; getting the position wrong).
         result (cond (map? form)  (into {} (map (fn [[k v]]
                                                   [k (walk-indexed (conj coor (pr-str k)) f v)])
                                                 form))
                      ;; Order of sets is unpredictable, unfortunately.
                      (set? form)  form
                      ;; Borrowed from clojure.walk/walk
                      (list? form) (apply list (map-inner form))
                      (instance? clojure.lang.IMapEntry form) (vec (map-inner form))
                      (seq? form)  (doall (map-inner form))
                      (coll? form) (into (empty form) (map-inner form))
                      :else form)]
     (f coor (merge-meta result (meta form))))))

(comment
  (def f (walk-indexed (fn [i f] (merge-meta f {:coor i})) '(foo (bar [baz]))))
  (clojure.walk/postwalk (juxt identity meta) f)

  (clojure.walk/postwalk
   (juxt identity meta)
   (reader/read
    (reader-types/source-logging-push-back-reader
     (reader-types/indexing-push-back-reader "(foo (bar))"))))

  (meta (reader-types/log-source
         (reader/read-string "(foo bar)"))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn read-indexed
  "via tools.reader"
  [file]
  (let [file-reader (reader-types/source-logging-push-back-reader
                     (reader-types/indexing-push-back-reader
                      (io/reader file)))]
    (take-while (partial not= ::end)
                (repeatedly #(reader/read file-reader nil ::end)))))

(defn with-coors [form]
  (walk-indexed (fn [i f] (merge-meta f {:coor i})) form))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn read-top-level-form-of-def-from-file
  "def is a def as found in an analyzed cljs ns (via :defs). file is a cljs file."
  [def file]
  (->> file
       read-indexed
       (filter #(= (:line def) (-> % meta :line)))
       first
       with-coors))

(defn find-nested-form-closest
  [line-to-find col-to-find form]
  ;; rank form by dist form `col-to-find`
  (let [forms (loop [node (ast/tree-zipper form) result []]
                (if (z/end? node) result
                    (let [form (z/node node)
                          {:keys [line column]} (meta form)
                          result (if (= line line-to-find)
                                   (conj result [(Math/abs (- column col-to-find))
                                                 form])
                                   result)]
                      (recur (z/next node) result))))]
    (->> forms
         (sort-by first <)
         (map second)
         first)))
