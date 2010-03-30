(ns corfox.q3.web
  (:use (clojure.contrib seq-utils)
	(clj-html core utils)))

(defn inner-html [x]
  (if (sequential? x) (apply str (interpose ", " x)) (str x)))

(defn html-atomic [tag x]
  (let [inner (inner-html x)
	start-tag (apply str ["<" tag ">"])
	end-tag (apply str ["</" tag ">"])]
    (apply str [start-tag inner end-tag])))

(defhtml li [x]
  [:li (inner-html x)])

(defhtml ul [coll]
  [:ul (map-str li coll)])

(declare table)

(defn cell [x]
  (if (sequential? x)
    (if (empty? x)
      (html [:td])
      (if (map? (first x))
	(html [:td (table x)])
	(html [:td (ul x)])))
    (html [:td (inner-html x)])))

(defhtml row [coll] 
  [:tr (map-str cell coll)])

(defn table [rs]
  (if (not (empty? rs))
    (let [headers (keys (first rs))]
      (html [:table (row headers) (map-str row (map vals rs))]))))

(defn accordion 
  [sections]
  (let [div-id (gensym)]
    (html
     [:script {:type "text/javascript"}
      (str
       "$(document).ready(function(){"
       "$(\"#" div-id "\").accordion();"
       "});")]
     [:div {:id div-id}
      (map-str
       (fn [[header content]]
	 (html
	  [:h3 [:a {:href "#"} header]]
	  [:div content]))
       (partition 2 sections))])))


(defn dynamic-tabs
  [headers content-fns tabsload-fns]
  (let [tab-id (gensym)]
    (html
     [:script {:type "text/javascript"}
      (str 
       "$(document).ready(function(){"
       "$(\"#" tab-id "\").tabs({"
       "load: function(event, ui) {"
       "alert(ui.tab);"
       "alert(ui.panel.id);"
       "alert(ui.index);"
       "}"
       "});"
       "});")]
     [:div {:id tab-id}
      [:ul
       (apply str
	      (map
	       (fn [header content-fn]
		 (html [:li [:a {:href (content-fn tab-id)} [:span header]]]))
	       headers
	       content-fns))]])))
