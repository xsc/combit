(ns ^{ :doc "NAND based logical gates."
       :author "Yannick Scherer" }
  combit.examples.nand-logic
  (:use combit.core
        [combit.combination :as combine]))

;; ## NAND Universal Gate

(def-primitive nand-gate [a b]
  (if (= (+ a b) 2) 0 1))

;; ## Derived Gates
;; see: http://en.wikipedia.org/wiki/NAND_logic

(def-gate not-gate [a] [out]
  (>> [(a) (a)] 
      (nand-gate) 
      (out)))

(def-gate and-gate [a b] [out]
  (>> [(a) (b)] 
      (nand-gate) 
      (not-gate) 
      (out)))

(def-gate or-gate [a b] [out]
  (>> [(a) (b)]
      (combine/parallel-1 (not-gate) (not-gate))
      (nand-gate)
      (out)))
       
(def-gate nor-gate [a b] [out]
  (>> [(a) (b)]
      (or-gate)
      (not-gate)
      (out)))

(def-gate xor-gate [a b] [out]
  (>> [(a) (b)]
      (combine/parallel-1
        (combine/prepend-inputs (nand-gate (b)))
        (combine/prepend-inputs (nand-gate (a))))
      (combine/parallel-2 (nand-gate) (nand-gate))
      (nand-gate)
      (out)))

(def-gate xnor-gate [a b] [out]
  (>> [(a) (b)]
      (xor-gate)
      (not-gate)
      (out)))
