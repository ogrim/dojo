(ns dojo.tennis
  (:require [clojure.string :refer [lower-case]]))

;; Coding Dojo - Capgemini Stavanger - 09.09.2014

;; Team members:
;;  Gunnstein Økland
;;  Morten Aasbak
;;  Knut Mathiesen
;;  Aleksander Skjæveland Larsen

(defn i->t [i]
  (case i
    0 "love"
    1 "15"
    2 "30"
    3 "40"
    i))

(defn print-score [{:keys [a b]}]
  (cond (and (= a 1) (= b 1)) "15-all"
        (and (= a 2) (= b 2)) "30-all"
        (and (= a b) (>= a 3)) "deuce"
        (and (< a 4) (< b 4)) (str (i->t a) "-" (i->t b))
        (= (- a b) 1) "advantage player a"
        (= (- b a) 1) "advantage player b"
        (> (- a b) 1) "game player a"
        (> (- b a) 1) "game player b"))

(defn game-over? [{:keys [a b]}]
  (and (or (> a 3) (> b 3))
       (> (Math/abs (- a b)) 1)))      

(defn score [state player]
  (if (game-over? state) state
      (let [new-state (update-in state [player] inc)]
        (println (print-score new-state))
        new-state)))

(defn play-game [scores]
  (reduce score {:a 0 :b 0} (map #(keyword (lower-case (str %))) scores)))
