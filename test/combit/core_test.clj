(ns combit.core-test
  (:use clojure.test
        combit.core))

;; ## Simple Element Transfer

(def-component swap-halves [in 4] [out 4]
  (>> (in 0 1) (out 2 3))
  (>> (in 2 3) (out 0 1)))

(def-component doubler [in 2] [out 4]
  (>> (in) (out 0 1))
  (>> (in) (out 2 3)))

(def-component split-halves [in 4] [upper 2 lower 2]
  (>> (in 0 1) (upper))
  (>> (in 2 3) (lower)))

(def-component join-halves [upper 2 lower 2] [out 4]
  (>> (upper) (out 0 1))
  (>> (lower) (out 2 3)))

(deftest simple-component-tests
  (testing "Single input, single output"
    (are [in out] (= (run swap-halves in) [out])
         [1 2 3 4] [3 4 1 2]
         [0 0 1 1] [1 1 0 0])
    (are [in out] (= (run doubler in) [out])
         [1 2] [1 2 1 2]
         [0 1] [0 1 0 1]))
  (testing "Single input, multiple outputs"
    (are [in a b] (= (run split-halves in) [a b])
         [1 2 3 4] [1 2] [3 4]
         [0 0 1 1] [0 0] [1 1]))
  (testing "Multiple inputs, single output."
    (are [a b out] (= (run join-halves a b) [out])
         [1 2] [3 4] [1 2 3 4]
         [0 0] [1 1] [0 0 1 1])))

;; ## Combination of Components

(def-component swap-and-split1 [in 4] [a 2 b 2]
  (>> (in)
      (swap-halves)
      (split-halves)
      [(a) (b)]))

(def-component swap-and-split2 [in 4] [a 2 b 2]
  (>> (swap-halves (in))
      (split-halves)
      [(a) (b)]))

(deftest component-combination-tests
  (testing "Combination of Components."
    (are [in a b] (= (run swap-and-split1 in) [a b])
         [1 2 3 4] [3 4] [1 2]
         [0 0 1 1] [1 1] [0 0])
    (are [in a b] (= (run swap-and-split2 in) [a b])
         [1 2 3 4] [3 4] [1 2]
         [0 0 1 1] [1 1] [0 0])))
