(ns corfox.q3.ofchart
  (:require [clojure.data.json :as json])
  (:use (hiccup core)))

; Expects jquery and swfobject to be loaded.
; Funny div at the end is to force end tag, 
; otherwise multiple charts per page fail.
(defn ofchart [data-file]
  (let [div-id (gensym)]
    (html
     [:script {:type "text/javascript"}
      (str
       "$(document).ready( function() {"
       "swfobject.embedSWF("
       "'/resources/open-flash-chart.swf', '" div-id "',"
       "'400', '200', '9.0.0', 'expressInstall.swf',"
       "{'loading':'Totting Frags...',"
       "'data-file':'" data-file "'}"
       ");"
       "});")]
     (str "<span class=\"chart\" id=\"" div-id "\"></span>"))))
;     (str "<div class=\"chart\" id=\"" div-id "\"></div>"))))

(defn ofchart-data [values labels colors]
  (json/write-str
   {"bg_colour" "#000000"
    "elements"
    [{"type" "pie"
      "alpha" 0.6
      "start-angle" 35
      "animate" [{"type" "fade"}]
      "tip" "#val# of #total# (#percent#)"
      "colours" colors
      "values" (map (fn [v l] {"value" v "label" l}) values labels)}]}))
