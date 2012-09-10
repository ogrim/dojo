(ns dojo.poker
  (:use [clojure.test])
  (:require [clojure.string :as str]))

; Coding Dojo - Capgemini Stavanger - 10.09.2012

; Team members:
;  Kristian Hiim
;  Anders Njøs Slinde
;  Daniel Nyvik (did a Quistling after 30 mins)
;  Aleksander Skjæveland Larsen

(def ts
  {"T" "10" , "J" "11" , "Q" "12" , "K" "13"
   "A" "14" , "2" "2" , "3" "3" , "4" "4"
   "5" "5" , "6" "6" , "7" "7" , "8" "8" , "9" "9"})

(defn value [[n t]]
  {:value (Integer/parseInt (ts (str n))) :suit (str t)})

(defn parse [s]
  (->> (str/split s #" ")
       (map value)
       (sort-by :value)))

(defn flush [v]
  (= 1 (count (distinct (map :suit v)))))

(defn straight [v]
  (let [b (map :value v)
        d (count (distinct b))]
    (and (= d 5) (= (- (last b) (first b)) 4))))

(defn straight-flush [v]
  (and (straight v) (flush v)))

(defn count-cards [v]
  (->> (group-by #(:value %) v)
       (map #(count (second %)))
       sort
       reverse))

(defn four-of-a-kind [v]
  (= 4 (first (count-cards v))))

(defn house [v]
  (let [[f s] (count-cards v)]
    (and (= f 3) (= s 2))))

(defn two-pair [v]
  (let [[f s] (count-cards v)]
    (and (= f 2) (= s 2))))

(defn one-pair [v]
  (= 2 (first (count-cards v))))

(defn three-of-a-kind [v]
  (= 3 (first (count-cards v))))

(def score-fns
  [straight-flush
   four-of-a-kind
   house
   flush
   straight
   three-of-a-kind
   two-pair
   one-pair])

(def resolvers
  {8 :straight-flush
   7 :four-of-a-kind
   6 :house
   5 :flush
   4 :straight
   3 :three-of-a-kind
   2 :two-pair
   1 :one-pair
   0 :high-card})

(defn score [v1 v2]
  (let [s1 (map #(% (parse v1)) score-fns)
        s2 (map #(% (parse v2)) score-fns)
        ss1 (drop-while false? s1)
        ss2 (drop-while false? s2)
        c1 (count ss1)
        c2 (count ss2)]
    (cond (> c1 c2) :v1
          (> c2 c1) :v2
          :else [v1 v2 (resolvers c1)])))

(defn resolve [v1 v2 ks]
  (cond (= ks :straight-flush) ()
        (= ks :straight) ()
        (= ks :flush) ()
        (= ks :high-card) ()
        :else nil))

(score "3H 9H 9S 9C KD" "3D 8H 8S 8C QD")

(def k "3D 9C KD QD 2H")
(def k1 "3D 9D KD QD 2D")
(def k2 "6D 9D 7D 8D TD")
(def k3 "9D 9H 9S 9C KD")
(def k4 "3D 9H 9S 9C KD")
(def k5 "3D 2H 9S 9C KD")
(def k6 "3D 3H 9S 9C 9D")

(deftest test1
  (is (= (flush (parse k)) false))
  (is (= (flush (parse k1)) true)))
