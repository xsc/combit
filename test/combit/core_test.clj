(ns combit.core-test
  (:use clojure.test
        combit.core))

(deftest component-tests
  (testing "Single input, single output"
    (let [swap-halves (component [in 4] [out 4]
                        (>> (in 0 1) (out 2 3))
                        (>> (in 2 3) (out 0 1)))]
      (are [in out] (= (swap-halves [in]) [out])
           [1 2 3 4] [3 4 1 2]
           [0 0 1 1] [1 1 0 0]))
    (let [doubler (component [in 2] [out 4]
                    (>> (in) (out 0 1))
                    (>> (in) (out 2 3)))]
      (are [in out] (= (doubler [in]) [out])
           [1 2] [1 2 1 2]
           [0 1] [0 1 0 1])))
  (testing "Single input, multiple outputs"
    (let [split-halves (component [in 4] [upper 2 lower 2]
                         (>> (in 0 1) (upper))
                         (>> (in 2 3) (lower)))]
      (are [in a b] (= (split-halves [in]) [a b])
           [1 2 3 4] [1 2] [3 4]
           [0 0 1 1] [0 0] [1 1])))
  (testing "Multiple inputs, single output."
    (let [join-halves (component [upper 2 lower 2] [out 4]
                        (>> (upper) (out 0 1))
                        (>> (lower) (out 2 3)))]
      (are [a b out] (= (join-halves [a b]) [out])
           [1 2] [3 4] [1 2 3 4]
           [0 0] [1 1] [0 0 1 1])))
  (testing "Combination of Components."
    (let [swap-halves (component [in 4] [out 4]
                        (>> (in 0 1) (out 2 3))
                        (>> (in 2 3) (out 0 1)))
          split-halves (component [in 4] [upper 2 lower 2]
                         (>> (in 0 1) (upper))
                         (>> (in 2 3) (lower)))
          swap-and-split (component [in 4] [a 2 b 2]
                           (>> (in) swap-halves split-halves [(a) (b)]))]
      (are [in a b] (= (swap-and-split [in]) [a b])
           [1 2 3 4] [3 4] [1 2]
           [0 0 1 1] [1 1] [0 0]))))

