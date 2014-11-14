(ns corfox.q3.charts
  (:use (corfox.q3 stats ofchart)))

(defmulti chart
  (fn [m] (:chartname m)))

(defn extract-player [m]
  (first (filter #(= (:name %) (:player m)) (players))))

(defmethod chart "player-kills-by-weapon" [m]
  (let [player (extract-player m)
	kills (kills-by-weapon player)]
    (ofchart-data (map second kills)
		  (map first kills)
		  (map #(nth % 3) kills))))

(defmethod chart "player-kills-by-victim" [m]
  (let [player (extract-player m)
	kills (kills-by-victim player)]
    (ofchart-data (map second kills)
		  (map first kills)
		  (map #(player-color %) (map first kills)))))

(defmethod chart "player-kills-by-map" [m]
  (let [player (extract-player m)
	kills (kills-by-map player)]
    (ofchart-data (map second kills)
		  (map first kills)
		  (map #(map-color %) (map first kills)))))

(defmethod chart "player-kill-death-ratio-by-map" [m]
  (let [player (extract-player m)
	kills (kills-by-map player)]
    (ofchart-data (map (fn [x] (nth x 3)) kills)
		  (map #(map-label (first %)) kills)
		  (map #(map-color %) (map first kills)))))

(defmethod chart "arena-kills-by-weapon" [m]
  (let [mapname (:mapname m)
	arena (:arena m)
	kills (arena-kills-by-weapon mapname arena)]
    (ofchart-data (map second kills)
		  (map first kills)
		  (map #(nth % 3) kills))))
