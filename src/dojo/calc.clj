(ns dojo.calc
  (:require [instaparse.core :as ip]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn add [s]
  (->> (str/split s #"[\n,]")
       (map edn/read-string)
       (reduce +)))

(def p1
  (ip/parser
   "S = <'//'> delimiter+ <'\n'> nums
    delimiter =  #'(.)+'
    nums = #'(.)+'"))

(defn addy [s]
  (let [[_ [_ delimiter] [_ nums]] (p1 s)]
    (->> (str/split nums (re-pattern delimiter))
         (map edn/read-string)
         (reduce +))))

(def p2
  (ip/parser
   "S = <'//'> delimiter+ <'\n'> nums
    delimiter = <'['> #'[a-zA-Z,.;:]+' <']'>
    nums = #'(.)+'"))
