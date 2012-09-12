(ns dojo.poker.refactored
  (:use [clojure.test])
  (:require [clojure.string :as str]))

(def card-table
  {"T" "10" , "J" "11" , "Q" "12" , "K" "13", "A" "14" , "2" "2"
   "3" "3" , "4" "4" ,"5" "5" , "6" "6" , "7" "7" , "8" "8" , "9" "9"})

(defn parse-card [[n t]]
  {:value (-> n str card-table Integer/parseInt) :suit (str t)})

(defn parse-hand [s]
  (->> (str/split s #" ")
       (map parse-card)
       (sort-by :value)))

(defn group-cards [hand]
  (->> (group-by :value hand)
       (map (comp count second))
       sort reverse))

(defn sorted-group-cards [hand]
  (let [result (group-by :value hand)
        comp-fn (fn [x y] (compare [(count (get result y)) y]
                                  [(count (get result x)) x]))]
    (into (sorted-map-by comp-fn) result)))

(defn same-suit? [hand]
  (= (-> (map :suit hand) distinct count) 1))

(defn match [in not-in]
  {:match true :in-rank in :not-in-rank not-in})

(def no-match (assoc (match [] []) :match false))

(defn flush [hand]
  (if (same-suit? hand)
    (match hand []) no-match))

(defn straight? [hand]
  (let [[f _ _ _ l :as vs] (map :value hand)
        dist (count (distinct vs))]
    (and (= dist 5) (= (- l f) 4))))

(defn straight [hand]
  (if (straight? hand)
    (match hand []) no-match))

(defn straight-flush [hand]
  (if (and (straight? hand) (same-suit? hand))
    (match hand []) no-match))

(defn four-of-a-kind [hand]
  (let [[f l] (sorted-group-cards hand)] [f l]
       (if (= (count (val f)) 4)
         (match (val f) (val l)) no-match)))

(defn house [hand]
  (let [[f s] (group-cards hand)]
    (if (and (= f 3) (= s 2))
      (match hand []) no-match)))

(defn two-pair [hand]
  (let [[f s] (group-cards hand)]
    (if (and (= f 2) (= s 2))
      (match (take 4 hand) [(last hand)]) no-match)))

(defn one-pair? [hand]
  (= 2 (first (group-cards hand))))

(defn three-of-a-kind? [hand]
  (= 3 (first (group-cards hand))))

(def score-fns
  [{:fn straight-flush :n 8}
   {:fn four-of-a-kind :n 7}
   {:fn house :n 6}
   {:fn flush :n 5}
   {:fn straight :n 4}
   {:fn three-of-a-kind :n 3}
   {:fn two-pair :n 2}
   {:fn one-pair :n 1}
   {:fn nil :n 0}])

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

(defn play [v1 v2]
  (let [cards1 (parse-hand v1)
        cards2 (parse-hand v2)
        s1 (->> (map #(% cards1) score-fns) (drop-while false?))
        s2 (->> (map #(% cards2) score-fns) (drop-while false?))
        c1 (count s1)
        c2 (count s2)]
    (cond (> c1 c2) :v1
          (< c1 c2) :v2
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
(def k7 "3D 3H 9S 9C 7D")

(deftest test1
  (is (= (flush (parse-hand k)) false))
  (is (= (flush (parse-hand k1)) true)))
