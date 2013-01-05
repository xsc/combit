(ns ^{ :doc "Test of Combit Core Functionality."
       :author "Yannick Scherer" }
  combit.core-test
  (:use clojure.test
        combit.core))

;; ## Simple Element Transfer

(def-component swap-halves [in 4] [out 4]
  (>>* (in 0 1) (out 2 3))
  (>>* (in 2 3) (out 0 1)))

(def-component doubler [in 2] [out 4]
  (>>* (in) (out 0 1))
  (>>* (in) (out 2 3)))

(def-component split-halves [in 4] [upper 2 lower 2]
  (>>* (in 0 1) (upper))
  (>>* (in 2 3) (lower)))

(def-component join-halves [upper 2 lower 2] [out 4]
  (>>* (upper) (out 0 1))
  (>>* (lower) (out 2 3)))

(def-component join-halves2 [upper 2 lower 2] [out 4]
  (>> (value (upper)) (out 0 1))
  (>> (value (lower)) (out 2 3)))

(deftest simple-component-tests
  (testing "Component Call Exceptions"
    (is (thrown-with-msg? Exception #"expected 1 data block\(s\)" 
                          (swap-halves [1] [1])))
    (is (thrown-with-msg? Exception #"expected 1 data block\(s\)" 
                          (split-halves [1] [1])))
    (is (thrown-with-msg? Exception #"expected 2 data block\(s\)" 
                          (join-halves [1] [1] [1])))
    (is (thrown-with-msg? Exception #"expected 2 data block\(s\)" 
                          (join-halves2 [1] [1] [1]))))
  (testing "Single input, single output"
    (are [in out] (= (swap-halves in) [out])
         [1 2 3 4] [3 4 1 2]
         [0 0 1 1] [1 1 0 0])
    (are [in out] (= (doubler in) [out])
         [1 2] [1 2 1 2]
         [0 1] [0 1 0 1]))
  (testing "Single input, multiple outputs"
    (are [in a b] (= (split-halves in) [a b])
         [1 2 3 4] [1 2] [3 4]
         [0 0 1 1] [0 0] [1 1]))
  (testing "Multiple inputs, single output."
    (are [a b out] (= (join-halves a b) [out])
         [1 2] [3 4] [1 2 3 4]
         [0 0] [1 1] [0 0 1 1])
    (are [a b out] (= (join-halves2 a b) [out])
         [1 2] [3 4] [1 2 3 4]
         [0 0] [1 1] [0 0 1 1])))

;; ## Calling Components

(def-component swap-and-split1 [in 4] [a 2 b 2]
  (>>* (in)
       (swap-halves)
       (split-halves)
       [(a) (b)]))

(def-component swap-and-split2 [in 4] [a 2 b 2]
  (>> (swap-halves (in))
      (split-halves)
      [(a) (b)]))

(deftest component-call-tests
  (testing "Combination of Components."
    (doseq [comp-fn [swap-and-split1 swap-and-split2]]
      (are [in a b] (= (comp-fn in) [a b])
           [1 2 3 4] [3 4] [1 2]
           [0 0 1 1] [1 1] [0 0]))))

;; ## Components with intermediate Results

(def-component split-and-swap1 [in 4] [a 2 b 2]
  (with-outputs [[upper lower] (split-halves (in))]
    (>>* (upper) (b))
    (>>* (lower) (a))))

(def-component split-and-swap2 [in 4] [a 2 b 2]
  (let [[upper lower] (split-halves (in))]
    (comp
      (>>* upper (b))
      (>>* lower (a)))))

(def-component split-and-swap3 [in 4] [a 2 b 2]
  (let [[upper lower] (split-halves (in))
        [u l] [(const upper) (const lower)]]
    (comp
      (>>* (u) (b))
      (>>* (l) (a)))))

(deftest component-intermediate-result-tests
  (testing "Intermediate Outputs."
    (doseq [comp-fn [split-and-swap1 split-and-swap2 split-and-swap3]]
      (are [in a b] (= (comp-fn in) [a b])
           [1 2 3 4] [3 4] [1 2]
           [0 0 1 1] [1 1] [0 0]))))

;; ## Gates

(def-gate swap-bits [a b] [x y]
  (>>* (a) (y))
  (>>* (b) (x)))

(def-gate and-bits [a b] [out]
  (>>* (vec (map bit-and (a) (b)))
       (out)))

(deftest gate-tests
  (testing "Gate Components."
    (is (= (swap-bits [0 1 0] [1 0 1]) (swap-bits [0] [1])))
    (are [a b x y] (= (swap-bits [a] [b]) [[x] [y]])
         1 0 0 1
         1 1 1 1
         0 0 0 0
         0 1 1 0)
    (is (= (and-bits [0 1 0] [1 0 1]) (and-bits [0] [1])))
    (are [a b out] (= (and-bits [a] [b]) [[out]])
         0 0 0
         0 1 0
         1 0 0
         1 1 1)))

;; ## Primitives

(def-primitive plus [a b]
  (+ a b))

(def-primitive mul [a b]
  (* a b))

(def-gate plus1-mul16 [in] [out]
  (>>* (in)
       (plus [1])
       (mul [16])
       (out)))

(deftest primitive-tests
  (testing "Primitive Components"
    (for [x (take 10 (repeatedly #(rand-int 100)))
          y (take 10 (repeatedly #(rand-int 100)))]
      (is (= (plus [x] [y]) [(+ x y)])))
    (for [x (take 10 (repeatedly #(rand-int 100)))
          y (take 10 (repeatedly #(rand-int 100)))]
      (is (= (mul [x] [y]) [(* x y)])))
    (for [x (take 10 (repeatedly #(rand-int 100)))]
      (is (= (plus1-mul16 [x]) [(* 16 (+ 1 x))])))))

