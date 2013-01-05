(ns ^{ :doc "A simple binary adder using Combit."
       :author "Yannick Scherer" }
  combit.examples.binary-adder
  (:use combit.core
        combit.combination
        [combit.data :as data]
        [combit.examples.nand-logic :as logic]))

(def-gate half-adder [a b] [s c]
  (>> [(a) (b)]
      (select-outputs [0 1 0 1])
      (parallel-2 
        (logic/xor-gate)
        (logic/and-gate))
      [(s) (c)]))

(def-gate full-adder [a b cin] [s c]
  (with-outputs [[s0 c0] (half-adder (a) (b))
                 [s1 c1] (half-adder (s0) (cin))]
    (>>* (s1) (s))
    (>> (or-gate (c0) (c1)) (c)))) 

(defn join-first 
  "Create component whose first input acts as an accumulator.
   The first result of the given component will be concatenated
   to said accumulator, whilst the second result will be passed
   as-is."
  [c]
  (sequential
    (prepend-inputs
      (sequential (select-outputs [1]) c))
    (select-outputs [0 2 3])
    (parallel [2 1]                       
      (fn [a b] [(data/concat-elements a b)])
      vector)))

(def-component add4 [a 4 b 4] [s 4 c 1]
  (>> [(a 3) (b 3)]
      (half-adder)
      (join-first (full-adder (a 2) (b 2)))
      (join-first (full-adder (a 1) (b 1)))
      (join-first (full-adder (a 0) (b 0)))
      [(s 3 0) (c)]))
