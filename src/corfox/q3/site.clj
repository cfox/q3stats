(ns corfox.q3.site
  (:use [corfox.q3 stats web views charts ofchart]
        compojure.core
        hiccup.core
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]))

(def my-meta [:meta {:http-equiv "Content-type" 
		     :content "text/html;charset=UTF-8"}])
 
(def tags {:xmlns "http://www.w3.org/1999/xhtml" :lang "en" :xml:lang "en"})
 
(defn head-setup
  [title]
  [:head
   [:title title]
   my-meta
   [:link {:type "text/css" :href "/resources/style.css" :rel "stylesheet"}]
   [:link {:type "text/css" :href "/css/dark-hive/jquery-ui-1.7.2.custom.css" 
	   :rel "stylesheet"}]
   [:script {:type "text/javascript" :src "/js/jquery-1.3.2.min.js"}]
   [:script {:type "text/javascript" :src "/js/jquery-ui-1.7.2.custom.min.js"}]
   [:script {:type "text/javascript" :src "/js/swfobject.js"}]
   ])
    
(defn page [title content]
  (html [:html tags (head-setup title)
	 [:body content]]))

(defroutes main-routes
  (GET "/" []
       (page "Hello!" [:div.panel [:h1 "Hello World"] 
;		       (chart "data.json")
;		       (chart "data2.json")
		       ]))

  (GET "/chart" request
       {:status 200
        :Content-type "application/json"
        :body (chart (:params request))})
;       [200 {:Content-type "application/json"} (chart (:params request))])

  (GET "/humans" []
       (page "Humans"
             [:div
              [:h1 "Humans Are The Best!"]
	      [:div.panel (human-index)]]))

  (GET "/players" []
       (page "Player Index" 
	     [:div
	      [:h1 "Player Index"]
	      [:div.panel (player-index)]]))

  (GET "/player/:name" [name]
       (page name
	     [:div
	      [:h1 name]
	      [:div.panel (player-detail name)]]))

  (GET "/maps" []
       (page "Map Index"
	     [:div
	      [:h1 "Maps"]
	      [:div.panel (map-index)]]))

  (GET "/maptab/:mapname/:tab-id" [mapname tab-id]
       (page (str mapname) " - " (map-label mapname))
	     (map-tab mapname tab-id))

  (GET "/weapons" []
       (page "Weapon Index"
	     [:div
	      [:h1 "Weapons"]
	      [:div (table (weapons))]]))
  
  ;(GET "*" (or (serve-file "." (params :*)) :next))
  ;(GET "/resources/*" (or (serve-file "./resources" (params :*)) :next))
  (route/files "/resources" {:root "resources"})
  (route/files "/css" {:root "css"})
  (route/files "/js" {:root "js"})
  
  ;(ANY "*" (page-not-found))
  (route/not-found "Page not found")

  )

(def app (-> (handler/site main-routes)
             (wrap-base-url)))

;; ========================================
;; The App
;; ========================================

;(defonce *app* (atom nil))

;(defn start-app []
;  (if (not (nil? @*app*))
;    (stop @*app*))
;  (reset! *app* (run-server {:port 8666}
;                            "/*" (servlet my-app))))

;(defn stop-app []
;  (when @*app* (stop @*app*)))
