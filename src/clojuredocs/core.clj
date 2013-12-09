;; This file was originally written by Zachary Kim (https://github.com/zk) and Xavier Riley (https://github.com/xavriley)
;; See https://github.com/zk/clojuredocs-analyzer/commits/master/src/cd_analyzer/core.clj for details on who did what :)

(ns clojuredocs.core
  (:use [clojuredocs.config]
        [clojuredocs.utils]
        [clojure.pprint :only (pprint)]
        [clostache.parser])
  (:require [clojure.data.json :as json])
  (:import [java.io File FileReader]
           [clojure.lang LineNumberingPushbackReader]))


(defn to-var-map [v]
  "Turns a var into a map tailored to our specific needs."
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
  "Turn a namespace into a real cool map!"
  (fn [file]
    (when-let [ns (file-to-ns file)]
      (let [m (meta ns)]
        {:name (.name ns)
         :file file
         :doc (:doc m)
         :web-path (.replace (.getAbsolutePath file) (.getAbsolutePath root-path) "")
         :vars (map to-var-map (ns-to-vars ns))}))))

(defn parse-git [#^File git-root]
  "Return a map with helpful git info."
  {:web-src-dir (git-dir-to-web-src-dir git-root)
   :commit (git-dir-to-commit git-root)
   :site-url (git-dir-to-site-url git-root)})


(defn parse-repo [#^File root sub-dir metadata]
  "Make a big-ass map out of an entire repo."
  (let [git-map (parse-git (path root ".git"))
        project metadata
        project (assoc project :source-root sub-dir)
        project (assoc project :cljs (cljs-in (:source-root project)))
        project (assoc project :namespaces (map (parse-ns-map root) (:cljs project)))
        library (merge {:projects [project]} git-map metadata)]
    library))

(defn get-projects [lib]
  "Getter for the projects in a repo"
  (:projects lib))

(defn get-nss [lib]
  "Getter for the namespaces in a repo"
  (reduce concat (map :namespaces (:projects lib))))

(defn get-vars [lib]
  "Get all the vars in all the namespaces in a repo.
  Thread the lib (a map of a repo) through each of these, i.e.
  (reduce concat (map :vars (reduce concat (map :namespaces (:projects lib)))))"
  (->> lib
       (:projects)
       (map :namespaces)
       (reduce concat)
       (map :vars)
       (reduce concat)))

(defn writefile [file text]
  "Write/overwrite a file with the supplied text."
  (with-open [w (clojure.java.io/writer  file :append false)]
    (.write w text)))

(defn report-on-lib [library]
  "For each namespace, and each var in each namespace, dump the map into a
  mustache template and write the corresponding files (and folders if necessary)."
  (time
    (try
      (let [projects (get-projects library)
            nss (get-nss library)
            vars (get-vars library)]

        ;; Create the dist folder and the folder named after the library
        ;; inside of it if they don't already exist
        (.mkdir (path "dist"))
        (.mkdir (path "dist" (:name library)))

        ;; Iterate over the projects in the lib
        (doseq [p (sort-by :name projects)]

          ;; ...and iterate over each namespace
          (doseq [ns (sort-by :name (:namespaces p))]
            ;; create a folder for the namespace
            (.mkdir (path "dist" (:name library) (:name ns)))

            ;; Iterate over the vars
            (doseq [v (sort-by :name (:vars ns))]
              (let [;; Store a stringified version of the var's foler name
                    folder-name (str "dist/" (:name library) "/" (:ns v) "/" (:name v))]

                ;; Create the folder for the var,
                ;; where the name of the folder is the var name
                (.mkdir (path "dist" (:name library) (:ns v) (:name v)))

                ;; Check to see if the examples.html exists for this var, and if not, create it.
                (if-not (#(.exists (clojure.java.io/as-file %)) (str folder-name "/examples.html"))
                  (writefile (str folder-name "/examples.html") (render-resource "templates/examples.html.mustache" v)))

                ;; Do the same for see-also.html
                (if-not (#(.exists (clojure.java.io/as-file %)) (str folder-name "/see-also.html"))
                  (writefile (str folder-name "/see-also.html") (render-resource "templates/see-also.html.mustache" v)))

                ;; Load the contents of the examples and see-also files
                (let [examples (slurp (str folder-name "/examples.html"))
                      see-also (slurp (str folder-name "/see-also.html"))]

                  ;; Create the var's html
                  (writefile (str folder-name "/index.html")
                             (render-resource "templates/var.html.mustache" (assoc v :examples examples
                                                                                     :see-also see-also)))))))))
      (catch Exception e
        (pprint "Import process failed: " e)))))


;;(slurp (str "dist/Clojure Core/clojure.edn/read/examples.html"))

(defn run-update-clojure-core [root-dir sub-dir]
  (report-on-lib (parse-repo (File. root-dir) (File. (str root-dir sub-dir)) core-meta)))

(defn run-update-clojure-contrib [root-dir sub-dir]
  (report-on-lib (parse-repo (File. root-dir) (File. (str root-dir sub-dir)) contrib-meta)))


(File. (str "/Users/masondesu/clojure/" "src/clj"))

(run-update-clojure-core "/Users/masondesu/clojure/" "src/clj")
#_(run-update-clojure-contrib "/Users/masondesu/clojurelibs/clojure-contrib")
