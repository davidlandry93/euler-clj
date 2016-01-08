(ns euler-clj.core
  (:gen-class))

(defn divides? [a b]
  (= (mod b a) 0))

(defn sieve [a confirmed-primes]
  (let [f (first a)]
    (if (> (* f f) (last a))
      (concat confirmed-primes a)
      (recur
       (filter (fn [x] (not (divides? f x))) a)
       (concat confirmed-primes [f])))))

(defn factor? [a b]
  "Is b a factor of a"
  (= (mod a b) 0))

(defn prime? [n]
  (cond
    (< n 2) false
    (= n 2) true
    :else (empty?
           (->>
            (range 2 (inc (Math/sqrt n)))
            (filter (partial factor? n))))))


(def primes
  "list all primes smaller than n"
  (filter prime? (range)))

(defn problem5-property? [m n]
  "Is m evenly divisible by every number up to n"
  (every?
   (fn [x] (factor? m x))
   (drop 1 (range n) )))

(defn problem5 [n]
  (first (filter (fn [x] (problem5-property? x n)) (drop 1 (range)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
