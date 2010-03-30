(ns corfox.q3.jgchart
  (:use (clojure.contrib seq-utils)
	(clj-html core utils)))

(defn jg-data [vec]                                                     
  (let [helper (fn [v] 
		 (apply str (concat ["["] (interpose "," v) ["]"])))]
    (if (empty? vec)
      "[]"
      (if (sequential? (first vec))
	(helper (map helper vec))
	(helper vec)))))

(defn jg-string-data [vec]
  (let [helper (fn [v]
		 (apply str 
			(concat ["["] (interpose "," (map #(str "'" % "'") v))
				["]"])))]
    (if (empty? vec) 
      "[]"
      (helper vec))))

(defn jgchart [vec, type, axis-labels, colors]
  (let [id (gensym)]
    (html
     [:script {:type "text/javascript"}
      (str
       "$(document).ready(function(){"
       "var api = new jGCharts.Api(); "
       "jQuery('<img>')"
       ".attr('src', api.make({data : "
       (jg-data vec)
       ", type : '"
       type
       "', axis_labels : "
       (jg-string-data axis-labels)
       ", size : '400x200', colors : "
       (jg-string-data colors)
       "}))"
       ".appendTo(\"#"
       id
       "\");"
       "});")]
     [:div {:id id}])))
