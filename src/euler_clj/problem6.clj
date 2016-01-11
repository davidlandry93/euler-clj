
(ns euler-clj.problem6
  (:require [euler-clj.core :as core])
  (:gen-class))

(defn question6 [n]
  (-
   (let [x (apply + (range n))] (* x x))
   (apply + (map (fn [x] (* x x)) (range n)))))
