(ns euler-clj.problem5
  (:require [ euler-clj.core :as core ])
  (:gen-class))

(defn divide-if-evenly-divisible [a b]
  "divide a by b if b is a factor of a"
  (if (core/factor? a b) (/ a b) a))

(defn lcm [n]
  (loop [ns n multiples-used [] primes-left core/primes]
    (let [new-factor (first primes-left)]
      (cond
        (every? (partial = 1) ns)
        (apply * multiples-used)
        (not (nil? (some (fn [x] (core/factor? x new-factor)) ns)))
        (recur
         (map (fn [x] (divide-if-evenly-divisible x new-factor)) ns)
         (conj multiples-used new-factor)
         primes-left)
        :else
        (recur
         ns
         multiples-used
         (drop 1 primes-left))))))

(defn problem5-v2 [n]
  (lcm (range 1 n)))
