(ns picture-language.core
  (:import [processing.core PImage])
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def img (ref nil))

(defn beside 
  "Will put an image to the right of the first one"
  [& imgs]
  (let [new-width  (->> imgs
                        (map #(.width %1))
                        (reduce +))
        new-height (->> imgs
                        (map #(.height %1))
                        (reduce +))
        new-image (q/create-image new-width new-height :rgb)]
    (.loadPixels new-image)
    (loop [img (first imgs)
           dest-x 0
           ]
      (.copy new-image img 
             0 0 (.width img) (.height img)
             dest-x 0 (.width img) (.height img))
      (recur (rest imgs) (+ dest-x (.width img))))
    (.updatePixels new-image)))
    
(defn setup 
  []
  (q/background 0)
  (dosync (ref-set img (q/load-image "res/gagarin.jpg")))
  (Thread/sleep 1000)
  (beside @img @img)
  )

(defn draw []
  (q/image @img 0 0))

(q/defsketch picture-language
  :title "image demo"
  :setup setup
  :draw draw
  :size [1000 1000])
