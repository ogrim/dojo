(ns dojo.gol-data)

(defn get-n [[x y] world]
  (nth (nth world y) x))

(defn world->coordinates [world]
  (let [coordinates (for [x (range (count world)) y (range (count world))
                          :when (= (get-n [x y] world) 1)]
                      [x y])
        offset (-> coordinates flatten distinct sort first)]
    (->> coordinates (map (fn [[x y]] [(- x offset) (- y offset)])) (into #{}))))

;;;;;;;;;;; Still life

(def block
  [[0 0 0 0]
   [0 1 1 0]
   [0 1 1 0]
   [0 0 0 0]])

(def beehive
  [[0 0 0 0 0 0]
   [0 0 1 1 0 0]
   [0 1 0 0 1 0]
   [0 0 1 1 0 0]
   [0 0 0 0 0 0]
   [0 0 0 0 0 0]])

(def loaf
  [[0 0 0 0 0 0]
   [0 0 1 1 0 0]
   [0 1 0 0 1 0]
   [0 0 1 0 1 0]
   [0 0 0 1 0 0]
   [0 0 0 0 0 0]])

(def boat
  [[0 0 0 0 0]
   [0 1 1 0 0]
   [0 1 0 1 0]
   [0 0 1 0 0]
   [0 0 0 0 0]])

;;;;;;;;;;; Oscillators

(def blinker
  [[0 0 0 0 0]
   [0 0 1 0 0]
   [0 0 1 0 0]
   [0 0 1 0 0]
   [0 0 0 0 0]])

(def toad
  [[0 0 0 0 0 0]
   [0 0 0 1 0 0]
   [0 1 0 0 1 0]
   [0 1 0 0 1 0]
   [0 0 1 0 0 0]
   [0 0 0 0 0 0]])

(def beacon
  [[0 0 0 0 0 0]
   [0 1 1 0 0 0]
   [0 1 1 0 0 0]
   [0 0 0 1 1 0]
   [0 0 0 1 1 0]
   [0 0 0 0 0 0]])

(def pulsar
  [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0]
   [0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0]
   [0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 1 1 1 0 0 1 1 0 1 1 0 0 1 1 1 0]
   [0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 0 0]
   [0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0]
   [0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 0 0]
   [0 1 1 1 0 0 1 1 0 1 1 0 0 1 1 1 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0]
   [0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0]
   [0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

;;;;;;;;;;; SPAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACE

(def glider
  [[0 0 0 0 0]
   [0 0 1 0 0]
   [0 0 0 1 0]
   [0 1 1 1 0]
   [0 0 0 0 0]])

(def lightweight-spaceship
  [[0 0 0 0 0 0 0]
   [0 1 0 0 1 0 0]
   [0 0 0 0 0 1 0]
   [0 1 0 0 0 1 0]
   [0 0 1 1 1 1 0]
   [0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0]])
