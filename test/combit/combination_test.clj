(ns ^{ :doc "Tests for Combit Component Combination"
       :author "Yannick Scherer" }
  combit.combination-test
  (:use clojure.test
        combit.core
        combit.combination))

;; ## Sequential

(def-component split-halves [in 4] [upper 2 lower 2]
  (>>* (in 0 1) (upper))
  (>>* (in 2 3) (lower)))

(def-component join-halves [upper 2 lower 2] [out 4]
  (>>* (upper) (out 0 1))
  (>>* (lower) (out 2 3)))

(def split-and-join 
  (sequential split-halves 
              join-halves))

(deftest sequential-combination-test
  (testing "Sequential Combination."
    (let [r (vec (take 4 (repeatedly #(rand-int 100))))]
      (is (= (split-halves r) [(take 2 r) (drop 2 r)]))
      (is (= (join-halves (take 2 r) (drop 2 r)) [r]))
      (is (= (split-and-join r) [r])))))

;; ## Parallel

;; Even

(def-component doubler [in 4] [a 4 b 4]
  (>>* (in) (a))
  (>>* (in) (b)))

(def split-two-inputs
  (parallel-1
    (split-halves)
    (split-halves)))

(def-component split-twice [in 4] [u1 2 l1 2 u2 2 l2 2]
  (>>* (in)
       (doubler)
       (split-two-inputs)
       [(u1) (l1) (u2) (l2)]))

;; Uneven

(def-component split-uneven [in 4] [a 1 b 1 c 2]
  (>>* (in 0) (a))
  (>>* (in 1) (b))
  (>>* (in 2 3) (c)))

(def-component pad-to-three [a 1 b 1] [out1 3 out2 3]
  (>>* [0 0] #{(out1 1 2) (out2 1 2)})
  (>>* (a) (out1 0))
  (>>* (b) (out2 0)))

(def-component invert [in 2] [out 2]
  (>>* (in 1 0) (out)))

(def pad1-invert2
  (parallel [2 1]
    pad-to-three
    invert))

(def-component split-pad1-invert2 [in 4] [a 3 b 3 c 3]
  (>>* (in)
       (split-uneven)
       (pad1-invert2)
       [(a) (b) (c)]))

;; Tests

(deftest parallel-combination-test
  (testing "Parallel Combination with same-size Inputs."
    (let [r (vec (take 4 (repeatedly #(rand-int 100))))]
      (is (= (doubler r) [r r]))
      (is (= (split-two-inputs r r)
             [(take 2 r) (drop 2 r)
              (take 2 r) (drop 2 r)]))
      (is (= (split-twice r) 
             [(take 2 r) (drop 2 r)
              (take 2 r) (drop 2 r)]))))
  (testing "Parallel Combination with differently sized Inputs."
    (let [r (vec (take 4 (repeatedly #(rand-int 100))))
          a (vector (first r))
          b (vector (second r))
          c (vec (drop 2 r))]
      (is (= (split-uneven r) [a b c]))
      (is (= (pad-to-three a b) [[(first r) 0 0] [(second r) 0 0]]))
      (is (= (invert c) [(reverse c)]))
      (is (= (pad1-invert2 a b c) [[(first r) 0 0] [(second r) 0 0] (reverse c)]))
      (is (= (split-pad1-invert2 r) [[(first r) 0 0] [(second r) 0 0] (reverse c)])))))


