(ns euler-clj.problem7
  (:require [ euler-clj.core :as core ])
  (:gen-class))

(take 1 (drop 10000 core/primes))
