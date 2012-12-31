(ns ^{ :doc "Combit Combination Operator"
       :author "Yannick Scherer" }
  combit.combine
  (:use [combit.utils :as u :only [throw-error]]))

;; The `>>` function combines transformations by supplying the output part emitted by a 
;; transformation as input to the subsequent one.
;;
;;    (>> A B C) ;=> (>> (>> A B) C)
;;    (>> A B) ;=> (fn [i o] (B (second (A i o)) o))

(defn combine
  "Combine a number of transformation functions into a new one, passing outputs from one
   transformation as inputs to the subsequent one."
  [& transformations]
  (reduce
    (fn [t1 t2]
      (fn [inputs outputs]
        (let [o (cond (fn? t1) (t1 inputs outputs)
                      (coll? t1) (mapcat (fn [t] (t inputs outputs)) t1)
                      :else (u/throw-error ">>" "Not a valid transformation: " t1))
              o (cond (fn? t2) (t2 o outputs)
                      (coll? t2) (reduce
                                   (fn [outputs [tx ox]]
                                     (tx [ox] outputs))
                                   outputs
                                   (map vector t2 o))
                      :else (u/throw-error ">>" "Not a valid transformation: " t2))]
          o)))
    transformations))
