(ns dojo.gol-refactored
  (:use [dojo.gol-data]))

(defn neighbours [[x y]]
  (for [x1 (range (dec x) (+ x 2)) y1 (range (dec y) (+ y 2))
        :when (not= [x1 y1] [x y])]  [x1 y1]))

(defn score [coordinates]
  (->> coordinates (mapcat neighbours) frequencies
       (filter (fn [[c s]] (or (= s 3) (and (= s 2) (coordinates c)))))
       (map first) set))

(def state (atom (world->coordinates glider)))

(defn tick [] (swap! state score))
