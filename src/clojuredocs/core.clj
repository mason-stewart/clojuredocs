;; This file was originally written by Zachary Kim (https://github.com/zk) and Xavier Riley (https://github.com/xavriley)
;; See https://github.com/zk/clojuredocs-analyzer/commits/master/src/cd_analyzer/core.clj for details on who did what :)

(ns clojuredocs.core
  (:use [clojuredocs.utils]
	      [clojure.pprint :only (pprint)]
        [clostache.parser])
  (:require [clojure.data.json :as json])
  (:import [java.io File FileReader]
	         [clojure.lang LineNumberingPushbackReader]))


(defn to-var-map [v]
  (let [{:keys [name ns doc line file arglists added]} (meta v)]
    {:name (str name)
     :ns (str ns)
     :doc (remove-leading-whitespace doc)
     :line line
     :file file
     :added added
     :source (source-for v)
     :arglists arglists
     :vars-in (map #(let [meta (meta %)]
		      {:ns (str (:ns meta))
		       :name (str (:name meta))}) (vars-in v))}))

(defn parse-ns-map [root-path]
  (fn [file]
    (when-let [ns (file-to-ns file)]
      (let [m (meta ns)]
	{:name (.name ns)
	 :file file
	 :doc (:doc m)
	 :web-path (.replace (.getAbsolutePath file) (.getAbsolutePath root-path) "")
	 :vars (map to-var-map (ns-to-vars ns))}))))

(defn parse-git [#^File git-root]
  {:web-src-dir (git-dir-to-web-src-dir git-root)
   :commit (git-dir-to-commit git-root)
   :site-url (git-dir-to-site-url git-root)})

(defn parse-clojure-core [#^File root]
  (let [git-map (parse-git (mkfile root ".git"))
	clojure-map {:name "Clojure Core"
		     :description "Clojure core environment and runtime library."
		     :site-url "http://clojure.org"
		     :copyright "&copy Rich Hickey.  All rights reserved."
		     :license "<a href=\"http://www.eclipse.org/legal/epl-v10.html\">Eclipse Public License 1.0</a>"
		     :version "1.5.1"
		     :source-root (mkfile root "src" "clj")}
	project clojure-map
	project (assoc project :cljs (cljs-in (:source-root project)))
	project (assoc project :namespaces (map (parse-ns-map root) (:cljs project)))
	library (merge {:projects [project]} git-map clojure-map)]
    library))

(defn parse-clojure-contrib [#^File root]
  (let [git-map (parse-git (mkfile root ".git"))
	cc-map {:name "Clojure Contrib"
			:description "The user contributions library, clojure.contrib, is a collection of namespaces each of which implements features that we believe may be useful to a large part of the Clojure community."
			:site-url "http://richhickey.github.com/clojure-contrib/"
			:copyright "&copy Rich Hickey.  All rights reserved."
			:license "<a href=\"http://www.eclipse.org/legal/epl-v10.html\">Eclipse Public License 1.0</a>"
			:version "1.2.0"
			:source-root (mkfile root "src" "main" "clojure")}
	project cc-map
	project (assoc project :cljs (cljs-in (:source-root project)))
	project (assoc project :namespaces (map (parse-ns-map root) (:cljs project)))
	library (merge {:projects [project]} git-map cc-map)]
    library))

(defn get-projects [lib]
  (:projects lib))

(defn get-nss [lib]
  (reduce concat (map :namespaces (:projects lib))))

(defn get-vars [lib]
  (->> lib
       (:projects)
       (map :namespaces)
       (reduce concat)
       (map :vars)
       (reduce concat)))

(defn writefile [file text]
  (with-open [w (clojure.java.io/writer  file :append false)]
    (.write w text)))

(defn report-on-lib [library]
  (let [start (System/currentTimeMillis)]
    (try
     (let [projects (get-projects library)
	         nss (get-nss library)
	         vars (get-vars library)]

       (doseq [p (sort-by :name projects)]
         (doseq [ns (sort-by :name (:namespaces p))]
           (doseq [v (sort-by :name (:vars ns))]
             (.mkdir (java.io.File. "dist"))
             (.mkdir (java.io.File. (str "dist/" (:name library))))
             (.mkdir (java.io.File. (str "dist/" (:name library) "/" (:ns v))))
             ;;(writefile (str "dist/" (:name library) (:ns v) "/" (:name v) ".json") (json/write-str v))
             (writefile (str "dist/" (:name library) "/" (:ns v) "/" (:name v) ".html") (render-resource "templates/var.html.mustache" {:name (:name v)}))))))
     (catch Exception e
       (pprint "Import process failed: " e)))))

(defn run-update-clojure-core [root-dir]
  (report-on-lib (parse-clojure-core (File. root-dir))))

(defn run-update-clojure-contrib [root-dir]
  (report-on-lib (parse-clojure-contrib (File. root-dir))))


(run-update-clojure-core "/Users/masondesu/clojure")
#_(run-update-clojure-contrib "/Users/masondesu/clojurelibs/clojure-contrib")
