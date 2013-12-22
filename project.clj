(defproject clojuredocs "0.0.0"
  :description "A fancier version of @zk's legendary http://clojuredocs.org"
  :url "http://masondesu.github.io/clojuredocs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [de.ubercode.clostache/clostache "1.3.1"]]
  :targets {:root-dir "/Users/masondesu/clojure/"
            :sub-dir  "src/clj"}
  :auto-analyzer {:src-dirs    ["src" "test"]
                  :sleep-time  1000}
  :eval-in-leiningen true)
