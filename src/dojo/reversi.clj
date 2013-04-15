(ns dojo.reversi)

(def initial-board
  #{[:black 3 3] [:black 4 4]
    [:white 3 4] [:white 4 3]})

(defn get-x [board x y]
  (let [b (get board [:black x y])
        w (get board [:white x y])]
    (cond b b
        w w
        :else nil)))

(defn neighours [x y]
  [[(dec x) y]
  [(inc x) y]
  [(dec x) (dec y)]
  [(inc x) (dec y)]
  [(dec x) (inc y)]
  [(inc x) (inc y)]
  [x (dec y)]
  [x (inc y)]])

(defn populated-neighbours [board x y]
  (let [color (first (get-x board x y))]
    (->> (neighours x y)
         (map (fn [[x y]] (get-x board x y)))
         (remove nil?))))

(defn get-color [board color]
  (filter #(= (first %) color) board))

(defn opposite [color]
  (if (= color :white) :black :white))

(defn direction [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])
;(direction [3 4] [4 4]) -> [1 0]

(defn next-neighbour [board current direction])

;(next-board initial-board :black)
;(get-x initial-board 4 4)

(defn color-of [board x y]
  (first (get-x board x y)))

;[:black 4 4]
;(get-color (populated-neighbours initial-board 4 4) :white)
;(map (fn [[_ x y]] (direction [4 4] [x y])) (get-color (populated-neighbours initial-board 4 4) :white))

(defn get-all-directions [board [x1 y1]]
  (map (fn [[_ x2 y2]] (direction [x1 y1] [x2 y2]))
       (get-color (populated-neighbours initial-board x1 y1)
                  (opposite (color-of board x1 y1)))))

; ([-1 0] [0 -1])
;; 4 4

(defn next-direction [position direction]
  [(+ (first direction) (first position)) (+ (second direction) (second position))])

(defn expand-direction [board position direction]
  (let [color (color-of board (first position) (second position))
        color2 (opposite color)]
    (loop [last-one (get-x board (next-direction position direction))]
      (cond (= (first last-one) color2) (recur )))))
