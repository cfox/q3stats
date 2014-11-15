(ns corfox.q3.stats
  (:use clojure.set)
  (:import (java.util Calendar)))

(def player-names
     {
      "Adam"    #{"A-Dub" "AdamBomb" "DivisionStreet" "Flashmob" "HateWave" "SexyFlanders" "The Mojo"
		  "Delerium Trigger" "The Despair Faction" "BadWolf" "Survivalism" "The Oncoming Storm"
		  "Born Slippy" "A" "a" "terrorhawk" "cerpin taxt" "Parasite" "Barking" "Spawn (Again)" 
		  "Util.java" "Legion of Boom" "The Beast" "Jeddy 3" "EonBlueApocalypse" "Killbot 2000" 
		  "Totalimmortal" "Bleed Black" "The Crowing" "One Armed Scissor" "Tiger Army"}
      "Darren"  #{"Fluffy Kitty" "Polymer" "Pookie" "Spolksky's Rectum"}
      "Corbin"  #{"Boom King" "Boom Wireless VPN King" "Heat Death of the Universe" "SCORBION"
		  "Jeff Atwood's Valet" "Magic Duck Sauce" "Multiplication Boulevard" "The Tough Brits" 
		  "Coprolalomaniacs Anonymous" "Fear and Thunder" "Peed Black"}
      "Justin"  #{"MB Scooby" "MBScooby" "MB_Scooby" "MB_Scooby2" "Turd Joyless Inn"}
      "Madan"   #{"Madx"}
      "Kevin"   #{"BigMac" "Sitting Duck" "Rail Meat"}
      "Charles" #{"w00t" "JoKeR" "JokeR" "n00b"}
      "Random"  #{"BlackGaff" "COBRA" "CamPeR" "Death" "EVILL" "FatFingers" "Foofi" "Graevyn" 
		  "Guitar Ninja" "Liquorished" "Omasal" 
		  "The Doctor" "W33NU$" "W33n13" "Zeke master" "UnnamedPlayer"
		  "^1Q^7a^1z^7a^1Q" "adam" "josh" "kingpin" "liquorish" "nazgul" "omasal" "xoxx" "xtreme"}
      })

(defn find-player
  [name]
  (let [hits (filter #((second %) name) player-names)]
    (if (empty? hits) "Unknown" 
	(if (> (count hits) 1) "Ambiguous"
	    (ffirst hits)))))

(def *game-log* 
     "games.log")

(defn map-seq
  "Given a sequence of lines from a q3 log file, return a sequence of maps (line groups)."
  [lines]
    (let [delimiter #(re-matches #".*----------.*" %)] 
      (filter #(not (some delimiter %)) 
              (partition-by delimiter lines))))

(defn played-map-seq
  "Given a sequence of lines from a q3 log file, return a sequence of played maps (line groups)."
  [lines]
  (filter #(some (fn [l] (re-matches #".*ClientConnect.*" l)) %) 
	  (map-seq lines)))

(defn seconds-between
  "Get number of seconds elapsed between t1 and t2.  Use 'HH24:MM:SS' format."
  [t1, t2]
  (let [rel-time 
	(fn [t]
	  (let [[h m s] (map #(Integer/parseInt %) 
			     (rest (re-matches #"(..):(..):(..)" t)))
		c (doto (Calendar/getInstance)
		    (.set (Calendar/HOUR_OF_DAY) h)
		    (.set (Calendar/MINUTE) m)
		    (.set (Calendar/SECOND) s))]
	    (/ (. c getTimeInMillis) 1000)))]
    (int (- (rel-time t2) (rel-time t1)))))
		     
(defn timecode
  "Get timecode from a line in a q3 log."
  [l]
  (second (re-matches #"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\].*" l)))

(def events #{"InitGame", "ClientBegin", "ClientUserinfoChanged", 
	      "Kill", "say", "sayteam", "Exit", "score", "ClientDisconnect"})

;Parse a line from a q3 log file, returning pertinent info in a map.
(defmulti
  parse-line
  (fn [l] (second (re-matches #"\[[0-9]{2}:[0-9]{2}:[0-9]{2}\] (\w+):.*" l))))

(defmethod parse-line "InitGame" [l]
  (let [[timecode mapname] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] InitGame: .*mapname\\(\w+)\\.*"
                                  l))]
    {:event "InitGame" :timecode timecode :mapname mapname}))

(defmethod parse-line "ClientBegin" [l]
  (let [[timecode id name ip] (rest (re-matches 
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] ClientBegin: ([0-9]+) (.*) \(([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)\:[0-9]+\)"
				     l))]
    {:event "ClientBegin" :timecode timecode :id id :name name :ip ip :player-name (find-player name)}))

(defmethod parse-line "ClientUserinfoChanged" [l]
  (let [[timecode id name model arena] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] ClientUserinfoChanged: ([0-9]+) .\\([^\\]+)\\.*\\model\\([^/]+)/.*\\([0-9])\\\w+\\[0-9]+$"
                                        l))]
    {:event "ClientUserinfoChanged" :timecode timecode 
     :id id :name name :model model :arena arena}))

(defmethod parse-line "Kill" [l]
  (let [[timecode id victim-id weapon-id arena weapon] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] Kill: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+).* by (.*)"
                                                 l))]
    {:event "Kill" :timecode timecode :id id :victim-id victim-id 
     :weapon-id weapon-id :arena arena :weapon weapon}))

(defmethod parse-line "say" [l]
  (let [[timecode id arena msg] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] say: ([0-9]+) ([0-9]+):.*: (.*)"
                                       l))]
    {:event "say" :timecode timecode :id id :arena arena :msg msg}))

(defmethod parse-line "sayteam" [l]
  (let [[timecode id arena msg] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] sayteam: ([0-9]+) ([0-9]+):.*: (.*)"
                                       l))]
    {:event "sayteam" :timecode timecode :id id :arena arena :msg msg}))

(defmethod parse-line "Exit" [l]
  (let [[timecode] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] Exit:.*"
                          l))]
    {:event "Exit" :timecode timecode}))

(defmethod parse-line "score" [l]
  (let [[timecode score id] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] score: ([0-9]+) .* client: ([0-9]+) .*"
                                   l))]
    {:event "score" :timecode timecode :score score :id id}))

(defmethod parse-line "ClientDisconnect" [l]
  (let [[timecode id] (rest (re-matches
#"\[([0-9]{2}:[0-9]{2}:[0-9]{2})\] ClientDisconnect: ([0-9]+).*"
                             l))]
    {:event "ClientDisconnect" :timecode timecode :id id}))

(defmethod parse-line :default [_] nil)

(defn parse-map [m mapid]
  (map conj
       (filter #(not (nil? %)) (map parse-line m))
       (repeat mapid)))

(defn parse-maps-once
  "Parse the palyed games from the game log into stats."
  []
  (let [map-data (ref [])]
    (with-open [rdr (clojure.java.io/reader *game-log*)]
      (doseq [inputs (map list
                          (played-map-seq (line-seq rdr))
                          (iterate (fn [v] [(first v) (inc (second v))])
                                   [:mapid 1]))]
        (dosync
         (alter map-data conj (parse-map (first inputs) (second inputs))))))
    (deref map-data)))
  
(def parse-maps (memoize parse-maps-once))

(defn generate-database []
     (apply hash-map
	    (flatten
	     (map (fn [x] [(:event (first x)) x])
		  (map set 
		       (partition-by (fn [x] (:event x)) 
				     (sort (fn [x y] (compare (:event x) (:event y))) 
					   (flatten (parse-maps)))))))))

(def database (memoize generate-database))

(defn get-players []
  (let [player-groups (group-by :player-name ((database) "ClientBegin"))
	get-names (fn [player-connections] 
		    (sort (fn [x y] (compare (second y) (second x))) 
			  (frequencies (map :name player-connections))))
	get-model (fn [n] 
		    (first 
		     (first 
		      (sort 
		       (fn [x y] 
			 (compare (second y) (second x))) 
		       (frequencies 
			(map :model 
			     (filter #(= (:name %) n) 
				     ((database) "ClientUserinfoChanged"))))))))]
    (sort (fn [x y] (compare (:maps-played y) (:maps-played x)))
	  (filter #(>= (:maps-played %) 1)
		  (map (fn [[player-name player-connections]]
			 (let [names (get-names player-connections)
			       primary-name (first (first names))]
			   {:player-name player-name
			    :name primary-name 
			    :maps-played (count player-connections)
			    :model (get-model primary-name)
			    :akas (map first (rest names))
			    }))
		       player-groups)))))

(def players (memoize get-players))

(defn map-number [m] 
  (Integer/parseInt (second (re-matches #"ra3map([0-9]+)$" (:mapname m)))))

(defn mapsort [m1 m2] 
  (< (map-number m1) (map-number m2)))

(defn maps []
  (let [view (join ((database) "InitGame") ((database) "Kill") {:mapid :mapid})
	mapname-groups (filter #(re-matches #"ra3map([0-9]+)$" (first %))
			       (group-by :mapname view))]
    (sort mapsort
	  (map (fn [[mapname rows]]
		 {:mapname mapname
		  :arenas (map (fn [[arenaid arenarows]] 
				 {:arena arenaid :kills (count arenarows)}) 
			       (group-by :arena rows))
		  :kills (count rows)})
	       mapname-groups))))

(defn weapons []
  (let [v (sort (distinct (map (fn [x] [(Integer/parseInt (:weapon-id x)) (:weapon x)]) 
			       ((database) "Kill"))))
	weapon-kills (fn [wid] 
		       (count 
			(filter #(= (Integer/parseInt (:weapon-id %)) wid) 
				((database) "Kill"))))]
    (reverse (reduce conj (list) (map (fn [[weapon-id weapon]]
					{:weapon-id weapon-id :weapon weapon
					 :kills (weapon-kills weapon-id)})
			     v)))))

(defn get-map-kills [m]
  (let [{:keys [mapname timecode]} (first (filter #(= (:event  %) "InitGame")
                                                  m))
	update-p (fn [p e]
		   (if (= (:event e) "ClientBegin")
		     (conj p {(:id e) (:player-name e)})
		     p))
	update-k (fn [k p e]
		   (if (= (:event e) "Kill")
		     (cons 
		      (dissoc 
		       (conj e {:player-name (p (:id e))
				:victim-player-name (p (:victim-id e))
				:mapname mapname
				:nth-second (seconds-between timecode (:timecode e))})
		       :event :id :victim-id)
		      k)
		     k))]
    (loop [p {"1022" "Environment"}
	   k (list)
	   e (first m)
	   r (rest m)]
      (if (empty? r)
	k
	(recur (update-p p e) (update-k k p e) 
	       (first r) (rest r))))))

(defn get-kills []
  (flatten (map get-map-kills (parse-maps))))

(def kills (memoize get-kills))

(defn player-kills
  [player]
  (filter #(= (:player-name %) (:player-name player)) (kills)))

(defn player-deaths
  [player]
  (filter #(= (:victim-player-name %) (:player-name player)) (kills)))

(defn total-kills
  [player]
  (count (player-kills player)))

(defn total-deaths
  [player]
  (count (player-deaths player)))

(defn ratio-to-float
  [r]
  (Float/valueOf
   (String/format "%1.2f" (to-array [(float r)]))))

(defn kill-death-ratio
  [player]
  (ratio-to-float (/ (total-kills player) 
		     (max 1 (total-deaths player)))))

(defn kills-per-map
  [player]
  (ratio-to-float (/ (total-kills player) (:maps-played player))))

(def weapon-group-metadata
     #{{:group "Shotgun" :color "fc8f05"}
       {:group "Gauntlet" :color "f70403"}
       {:group "Machinegun" :color "fdfc04"}
       {:group "Grenade" :color "00c70b"}
       {:group "Rocket" :color "e50404"}
       {:group "Plasma" :color "c704f1"}
       {:group "Railgun" :color "17ff17"}
       {:group "Lightning" :color "fcfdac"}
       {:group "BFG" :color "8304ef"}
       {:group "Environment" :color "888888"}})

(def weapon-groups (filter #(not (= "Environment" %)) (map :group weapon-group-metadata)))

(def weapon-metadata
     #{{:weapon-id 1 :weapon "MOD_SHOTGUN" :group "Shotgun"}
       {:weapon_id 2 :weapon "MOD_GAUNTLET" :group "Gauntlet"}
       {:weapon_id 3 :weapon "MOD_MACHINEGUN" :group "Machinegun"}
       {:weapon_id 4 :weapon "MOD_GRENADE" :group "Grenade"}
       {:weapon_id 5 :weapon "MOD_GRENADE_SPLASH" :group "Grenade"}
       {:weapon_id 6 :weapon "MOD_ROCKET" :group "Rocket"}
       {:weapon_id 7 :weapon "MOD_ROCKET_SPLASH" :group "Rocket"}
       {:weapon_id 8 :weapon "MOD_PLASMA" :group "Plasma"}
       {:weapon_id 9 :weapon "MOD_PLASMA_SPLASH" :group "Plasma"}
       {:weapon_id 10 :weapon "MOD_RAILGUN" :group "Railgun"}
       {:weapon_id 11 :weapon "MOD_LIGHTNING" :group "Lightning"}
       {:weapon_id 12 :weapon "MOD_BFG" :group "BFG"}
       {:weapon_id 13 :weapon "MOD_BFG_SPLASH" :group "BFG"}
       {:weapon_id 15 :weapon "MOD_SLIME" :group "Environment"}
       {:weapon_id 17 :weapon "MOD_CRUSH" :group "Environment"}
       {:weapon_id 18 :weapon "MOD_TELEFRAG" :group "Environment"}
       {:weapon_id 19 :weapon "MOD_FALLING" :group "Environment"}
       {:weapon_id 20 :weapon "MOD_SUICIDE" :group "Environment"}
       {:weapon_id 22 :weapon "MOD_TRIGGER_HURT" :group "Environment"}})

(defn weapon-group
  [weapon]
  (:group (first (select #(= (:weapon %) weapon) weapon-metadata))))

(defn kills-by-weapon
  [player]
  (let [tks (total-kills player)]
    (map (fn [[wg ks]] 
	   (let [wgks (count ks)] 
	     [wg wgks (ratio-to-float (/ wgks tks)) 
	      (:color (first (select #(= (:group %) wg) 
				     weapon-group-metadata)))]))
	 (group-by #(weapon-group (:weapon %)) (player-kills player)))))

(defn player-kills-by-weapon 
  [wg]	
  (map (fn [p] 
	 (let [[_ total pct] (first (filter (fn [[x]] (= wg x)) 
                                            (kills-by-weapon p)))]
	   [(:name p) (or total 0) (or pct 0.0)]))
       (players)))

(defn weapon-group-image-path
  [wg]
  (str "/resources/" wg "h.jpg"))

(defn most-kills [wg] (first (first (sort #(> (second %1) (second %2))
					  (player-kills-by-weapon wg)))))

(defn highest-kill-pct [wg] (first (first (sort #(> (nth %1 2) (nth %2 2))
						(player-kills-by-weapon wg)))))

(defn weapon-badges-kills
  [player]
  (let [name (:name player)
	wgs (filter #(= (most-kills %) name) weapon-groups)]
    (reduce conj [] (map (fn [wg] {:name name
				   :weapon wg
				   :badge-type "Most Kills"
				   :description (str name " has the most kills with " wg "!")
				   :image (weapon-group-image-path wg)})
			 wgs))))

(defn weapon-badges-pct
  [player]
  (let [name (:name player)
	wgs (filter #(= (highest-kill-pct %) name) weapon-groups)]
    (reduce conj [] (map (fn [wg] {:name name
				   :weapon wg
				   :badge-type "Highest Kill Pct"
				   :description (str name " has the highest pct of kills with " wg "!")
				   :image (weapon-group-image-path wg)})
			 wgs))))

(defn get-weapon-badges
  []
  (concat 
   (flatten (map weapon-badges-kills (players)))
   (flatten (map weapon-badges-pct (players)))))

(def weapon-badges (memoize get-weapon-badges))

(defn get-excellence-badges
  ([] 
     (reduce concat (map get-excellence-badges (players))))
  ([player]
     (reduce concat (map (partial get-excellence-badges player) (parse-maps))))
  ([player parsed-map]
     (let [{:keys [player-name name]} player
	   kills (sort #(< (:nth-second %1) (:nth-second %2)) 
		       (filter #(= player-name (:player-name %)) 
			       (get-map-kills parsed-map)))
	   kill-pairs (partition 2 1 kills)
	   excellences (filter (fn [[k1 k2]] 
				 (<= (- (:nth-second k2) (:nth-second k1)) 2))
				 kill-pairs)
	   badge-builder (fn [[k1 k2]]
			   {:name name
			    :weapon1 (:weapon k1)
			    :weapon2 (:weapon k2)
			    :mapname (:mapname k1)
			    :arena (:arena k1)
			    :interval (- (:nth-second k2) (:nth-second k1))})]
       (reduce conj [] (map badge-builder excellences)))))

(def excellence-badges (memoize get-excellence-badges))

(defn weapon-color
  [weapon]
  (:color (first (select #(= (:group %) (weapon-group weapon)) 
			 weapon-group-metadata))))

(defn build-player-name-to-name-map []
     (reduce conj {} (map (fn [p] {(:player-name p) (:name p)}) (players))))

(def player-name-to-name (memoize build-player-name-to-name-map))

(defn kills-by-victim
  [player]
  (let [tks (total-kills player)]
    (map (fn [[v ks]]
	   (let [vks (count ks)]
	     [v vks (ratio-to-float (/ vks tks))]))
	 (group-by (fn [k] (get (player-name-to-name) (:victim-player-name k) "Other"))
		   (player-kills player)))))

(defn build-player-color-map []
  (reduce conj {} (map (fn [p c] {(:name p) c}) 
		       (players) 
		       (cycle (map :color weapon-group-metadata)))))

(def player-color-map (memoize build-player-color-map))

(defn player-color
  [name]
  (get (player-color-map) name "888888"))

(defn kills-by-map
  [player]
  (let [tks (total-kills player)]
    (map (fn [[m ks]]
	   (let [mks (count ks)
		 mds (count 
		      (filter 
		       (fn [k] (= (:mapname k) m))
		       (player-deaths player)))]
	     [m mks mds (ratio-to-float (/ mks (max mds 1)))]))
	 (group-by (fn [k] (:mapname k))
		   (player-kills player)))))

(defn build-map-color-map []
  (reduce conj {} (map (fn [m c] {(:mapname m) c}) 
		       (maps) 
		       (cycle (map :color weapon-group-metadata)))))

(def map-color-map (memoize build-map-color-map))

(defn map-color
  [name]
  (get (map-color-map) name "888888"))

(def map-labels
     ;1
     [["High Noon"
       ["Evolution" "Thunderstruck" "Canned Heat" "Theatre Of Pain"]]
      ;2
      ["Liquid Blue"
       ["Electric Head" "Somewhat Damaged" "Window Pain" "Shaken Not Stirred"]]
      ;3
      ["Tranquility Arena"
       ["Electrodome" "The Divide" "The Generator" "Industrial Buttress" 
	"AC's Temple"]]
      ;4
      ["The Citadels"
       ["Caer Mael" "Mars' Bastion" "Knight's Keep" "Castle Deathstalker"]]
      ;5
      ["Lost Faith"
       ["The Traps Of Fate" "Faith In Death" "The Last Gift" "Abbey Of Temptation"]]
      ;6
      ["Manic"
       ["Sanity Slipped" "Slightly Schizo" "Brain Broke" "Moon Man"]]
      ;7
      ["The Houses Of Infliction"
       ["House Of The Desolate" "House Of Decay" "House Of Blood" 
	"House Of Infliction"]]
      ;8
      ["Hal's Base Arena"
       ["Gleam" "Grit" "Grunge" "Gutteral" "Gaudy"]]
      ;9
      ["Gizbang"
       ["Cannery" "3 Story" "Q3Terrain" "Megan's Place"]]
      ;10
      ["Europa Rendezvous"
       ["Arrival" "Doomed" "N.N.Y." "Jupiter Rising"]]
      ;11
      ["All The Aces"
       ["Death Or Glory" "Dead And Gone" "Eat The Gun" "Overkill"]]
      ;12
      ["Frag Like An Egyptian"
       ["Smash" "Drunken Mummy" "Midlife Crisis" "Hen House"]]
      ;13
      ["Home Base Of The Assassins"
       ["The Stadium" "The Chromatic Death" "The Try-Out" "The Assassins Hide Out"]]
      ;14
      ["Mid Night"
       ["Mid Night" "Caffeine" "Nicotine" "Tetrahydrocannabinol"]]
      ;15
      ["Stormator Complex"
       ["Stormatorium" "Vertical Mayhem" "Storm Keep" "Dungeon Of Doom" 
	"Chaos Within"]]
      ;16
      ["The Edge"
       ["Kube" "Mine Entrance" "Strogg Kombat" "The Edge"]]
      ;17
      ["Black Moon"
       ["Darkness Rising" "Nocturnal Carnage" "Lurid Slumber" "Black Moon"]]
      ;18
      ["Temple Of Yog-Sothoth"
       ["Temple" "Archway" "Santicity" "Downward Spiral"]]
      ;19
      ["The Voices"
       ["Rooftop Rumble" "Nog's Place" "The Drain" "The Voices"]]
      ;20
      ["Chapter X"
       ["Fragma" "Remix" "Spacegib" "Fragaholic"]]])

(defn map-label [mapname]
  (first 
   (nth map-labels 
	(- (Integer/parseInt (second (re-matches #"ra3map([0-9]+)" mapname))) 
	   1))))

(defn arena-label [mapname arenaid]
  (nth
   (second
    (nth map-labels
	 (- (Integer/parseInt (second (re-matches #"ra3map([0-9]+)" mapname)))
	    1)))
   (- (Integer/parseInt arenaid) 1)))

(defn map-image-path [mapname]
  (str "/resources/" mapname "/levelshots/" mapname "_400x400.png"))

(defn arena-image-path [mapname arenaid]
  (str "/resources/" mapname "/arenashots/" mapname "_" arenaid ".png"))

(defn arena-kills
  [mapname arena]
  (filter #(and 
	    (= (:mapname %) mapname)
	    (= (:arena %) arena))
	  (kills)))

(defn total-arena-kills
  [mapname arena]
  (count (arena-kills mapname arena)))

(defn arena-kills-by-weapon
  [mapname arena]
  (let [tks (total-arena-kills mapname arena)]
    (map (fn [[wg ks]] 
	   (let [wgks (count ks)] 
	     [wg wgks (ratio-to-float (/ wgks tks)) 
	      (:color (first (select #(= (:group %) wg) 
				     weapon-group-metadata)))]))
	 (group-by #(weapon-group (:weapon %)) (arena-kills mapname arena)))))
