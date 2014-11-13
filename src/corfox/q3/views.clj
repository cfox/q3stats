(ns corfox.q3.views
  (:import (java.io File))
  (:use (corfox.q3 stats web ofchart)
	(clojure.contrib seq-utils)
	(compojure encodings str-utils)
	(clj-html core helpers)))

(def *resource-dir*
     "/home/cfox/personal/projects/q3stats/resources")

(defhtml name-link [name]
  [:h3 [:a {:href (str "/player/" name)} name]])

(defn model-img [player]
  (let [default-img "ranger-1.jpg"
	name (:name player)
	model (:model player)
	img-finder (fn [m] 
		     (filter
		      (fn [x]
			(re-matches (re-pattern (str "^" m ".*$")) x))
		      (. (File. (str *resource-dir* "/icons")) list)))
	candidates (img-finder model)
	img (if (empty? candidates)
	      default-img
	      (nth candidates (rem (Math/abs (.hashCode name))
				 (count candidates))))]
    (html
     [:img {:src (str "/resources/icons/" img)
	    :alt name}])))

(defn badges [player]
  (let [name (:name player)
	badges (filter #(= name (:name %)) (weapon-badges))
	badge-groups (group-by :badge-type badges)
	make-badge-cell (fn [b] (html [:td [:img {:src (:image b)
						  :width 24 :height 24
						  :alt (:description b)
						  :title (:description b)}]]))
	make-badge-row (fn [bs] (html [:tr (map-str make-badge-cell bs)]))]
    (map-str make-badge-row (vals badge-groups))))

(defn excel-badges [player]
  (let [excel-count (count (excellence-badges player))]
    (html [:table [:tr 
		   [:td [:img {:src "/resources/excellent_48.png" :alt "Excellent!" :title "Excellent!"}]]
		   [:td (str "x" excel-count)]]])))

(defhtml player-summary-row [player]
  [:tr.player-summary
   [:td [:table
	 [:tr [:td (model-img player)]]
	 [:tr [:td [:table (badges player)]]]
	 [:tr [:td (excel-badges player)]]]]
   [:td [:table
	 [:tr [:td (name-link (:name player))]]
	 [:tr [:td (if (not (empty? (:akas player)))
		     (str "AKA: " 
			  (apply str 
				 (interpose ", " 
					    (take 2 (:akas player))))))]]
	 [:tr [:td (str "Maps Played: " (:maps-played player))]]
	 [:tr [:td (str "Total Kills: " (total-kills player))]]
	 [:tr [:td (str "Total Deaths: " (total-deaths player))]]
	 [:tr [:td (str "Kills per Map: " (kills-per-map player))]]
	 [:tr [:td (str "Kill/Death Ratio: " (kill-death-ratio player))]]]]
   [:td (ofchart 
	 (str "/chart/player-kills-by-weapon?"
	      (urlencode (str "player=" (:name player)))))]
   [:td (ofchart 
	 (str "/chart/player-kills-by-victim?"
	      (urlencode (str "player=" (:name player)))))]
   ])
		   
(defhtml player-index []
  [:table
   (map-str player-summary-row 
	    (sort (fn [x y] (> (kill-death-ratio x) 
			       (kill-death-ratio y)))
		  (players)))])

(defhtml player-detail [name]
  [:table
   (player-summary-row (find-first #(= (:name %) name) (players)))])

(defhtml human-index []
  [:p "Kill all humans!"])

(defn map-summary-section [map]
  (let [mapname (:mapname map)]
    (html
     (map-str (fn [a] 
		(let [arena (:arena a)] 
		  (html [:span 
			 [:h4 (arena-label mapname arena)]
			 [:img {:src (arena-image-path mapname arena) 
				:alt (arena-label mapname arena)}]
			 (ofchart 
			  (str "/chart/arena-kills-by-weapon?"
			       (urlencode 
				(str "mapname=" mapname "&" "arena=" arena))))])))
	      (:arenas map)))))

(defn map-summary-header [map]
  (let [mapname (:mapname map)]
    (str mapname " - " (map-label mapname) " - " (:kills map) " Total Kills")))

(defhtml map-index-accordion
  []
  (let [displayed-maps (filter #(> (:kills %) 200) (maps))]
    (accordion (interleave 
		(map map-summary-header displayed-maps)
		(map map-summary-section displayed-maps)))))

(defn map-summary-tab [map tab-id]
  (let [mapname (:mapname map)]
    (html
     (map-str (fn [a] 
		(let [arena (:arena a)] 
		  (html [:span 
			 [:h4 (str (arena-label mapname arena) " - " (:kills ((:arenas map) arena)) " Kills")]
			 [:img {:src (arena-image-path mapname arena) 
				:alt (arena-label mapname arena)}]
			 (ofchart 
			  (str "/chart/arena-kills-by-weapon?"
			       (urlencode 
				(str "mapname=" mapname "&" "arena=" arena))))])))
	      (:arenas map)))))

(defn map-tab
  [mapname tab-id]
     (map-summary-tab (find-first #(= mapname (:mapname %)) (maps)) tab-id))

(defn map-url
  [mapname tab-id]
  (str "/maptab/" mapname "/" tab-id))

(defhtml map-index-tabs
  []
  (let [displayed-maps (take 3 (maps))]
    (dynamic-tabs
     (map map-summary-header displayed-maps)
     (map #(partial map-url (:mapname %)) displayed-maps)
     nil)))

(defn map-index
  []
  (map-index-accordion))
