(defproject q3stats "1.0.0-SNAPSHOT"
  :description "Q3 Stats Just For Fun"
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
		 [compojure "0.3.2"]
		 [enlive "1.0.0-SNAPSHOT"]
                 [org.incanter/incanter-full "1.2.0-SNAPSHOT"]
		 [clj-html "0.1.0"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]]
  :repositories {"clojure-releases" "http://build.clojure.org/releases"
                 "incanter" "http://repo.incanter.org"}
  :source-path "src"
)