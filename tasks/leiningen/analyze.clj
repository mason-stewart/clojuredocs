;; A number of the functions in this file were heavily inspired by Kyle Burton's margauto plugin. Thanks!
;; See: https://github.com/kyleburton/lein-margauto/blob/master/src/leiningen/margauto.clj

(ns leiningen.analyze
  "Analyze and build docs. Use 'lein analyze auto' for auto rebuild."
  (:require [clojuredocs.core :as core])
  (:use [clojure.pprint :only (pprint)]))

(defn source-files-seq [dirs]
  (map str
       (filter
        #(and
          (.isFile %1)
          (.endsWith (str %1) ".clj"))
        (mapcat file-seq (map (fn [s] (java.io.File. s)) dirs)))))

(defn mtime [f]
  (.lastModified (java.io.File. f)))

(defn take-directory-snapshot [dirs]
  (apply
   str
   (map
    (fn [f]
      (format "%s:%s\n" f (mtime f)))
    (source-files-seq dirs))))

(defn analyze
  "Run the analyzer"
  ([project]
   (let [root-dir (get-in project [:targets :root-dir])
         sub-dir  (get-in project [:targets :sub-dir])]
     (pprint (str "Building docs, hold tight... "))
     (core/run-update-clojure-core root-dir sub-dir)))

  ([project auto]
   (if (= auto "auto")
     (do (analyze project)
      (pprint "Watching for changes in source directories")
      (let [src-dirs   (get-in project [:auto-analyzer :src-dirs] ["src"])
            pause-time (get-in project [:auto-analyzer :sleep-time] 1000)]
        (loop [before (take-directory-snapshot src-dirs)
               after  before]
          (if-not (= after before)
            (analyze project))
          (Thread/sleep pause-time)
          (recur after (take-directory-snapshot src-dirs)))))
     (analyze project))))
