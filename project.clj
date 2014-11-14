(defproject q3stats "1.1.0-SNAPSHOT"
  :description "Q3 Stats Just For Fun"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
		 [compojure "1.2.1"]
		 [enlive "1.1.5"]
                 [incanter/incanter-core "1.5.5"]
                 [hiccup "1.0.5"]]
  :plugins [[cider/cider-nrepl "0.7.0"]
            [lein-ring "0.8.13"]]
  :ring {:handler corfox.q3.site/app}
)
