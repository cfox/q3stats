(ns corfox.q3.site)
(use 'compojure)
(use '(corfox.q3 stats web views charts ofchart))

(def my-meta [:meta {:http-equiv "Content-type" 
		     :content "text/html;charset=UTF-8"}])
 
(def tags {:xmlns "http://www.w3.org/1999/xhtml" :lang "en" :xml:lang "en"})
 
(defn head-setup
  [title]
  [:head
   [:title title]
   my-meta
   [:link {:type "text/css" :href "/resources/style.css" :rel "stylesheet"}]
   [:link {:type "text/css" :href "css/dark-hive/jquery-ui-1.7.2.custom.css" 
	   :rel "stylesheet"}]
   [:script {:type "text/javascript" :src "/js/jquery-1.3.2.min.js"}]
   [:script {:type "text/javascript" :src "/js/jquery-ui-1.7.2.custom.min.js"}]
   [:script {:type "text/javascript" :src "/js/swfobject.js"}]
   ])
    
(defn page [title content]
  (html [:html tags (head-setup title)
	 [:body content]]))

(defroutes my-app
  (GET "/resources/*" (or (serve-file "./resources" (params :*)) :next))

  (GET "/"
       (page "Hello!" [:div.panel [:h1 "Hello World"] 
		       (chart "data.json")
		       (chart "data2.json")
		       ]))

  (GET "/chart/:chartname"
       [200, {:Content-type "application/json"} (chart params)])

  (GET "/humans"
       (page "Humans"
             [:div
              [:h1 "Humans Are The Best!"]
	      [:div.panel (human-index)]]))

  (GET "/players"
       (page "Player Index" 
	     [:div
	      [:h1 "Player Index"]
	      [:div.panel (player-index)]]))

  (GET "/player/:name"
       (page (params :name)
	     [:div
	      [:h1 (params :name)]
	      [:div.panel (player-detail (params :name))]]))

  (GET "/maps"
       (page "Map Index"
	     [:div
	      [:h1 "Maps"]
	      [:div.panel (map-index)]]))

  (GET "/maptab/:mapname/:tab-id"
       (page (str (params :mapname) " - " (map-label (params :mapname)))
	     (map-tab (params :mapname) (params :tab-id))))

  (GET "/weapons"
       (page "Weapon Index"
	     [:div
	      [:h1 "Weapons"]
	      [:div (table (weapons))]]))
  
  (GET "*" (or (serve-file "." (params :*)) :next))

  (ANY "*"
       (page-not-found)))

;; ========================================
;; The App
;; ========================================

(defonce *app* (atom nil))

(defn start-app []
  (if (not (nil? @*app*))
    (stop @*app*))
  (reset! *app* (run-server {:port 8666}
                            "/*" (servlet my-app))))

(defn stop-app []
  (when @*app* (stop @*app*))
)
