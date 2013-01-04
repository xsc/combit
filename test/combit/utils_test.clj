(ns ^{ :doc "Tests for Combit Utils"
       :author "Yannick Scherer" }
  combit.utils-test
  (:use clojure.test
        combit.utils))

(deftest new-vector-test
  (testing "Empty Vector Creation."
    (let [c (rand-int 100)
          v (new-vector c)]
      (is (= (count v) c))
      (is (every? nil? v)))
    (let [c (rand-int 100)]
      (doseq [initial [nil :a 0 identity]]
        (let [v (new-vector c initial)]
          (is (= (count v) c))
          (is (every? #(= initial %) v)))))))

(deftest throw-error-test
  (testing "Exception Generator."
    (is (thrown? Exception (throw-error "ERROR" "what?")))
    (is (thrown-with-msg? Exception #"\[ERROR\] what\?" 
                          (throw-error "ERROR" "what?")))
    (is (thrown-with-msg? Exception #"\[ERROR\] what :now?" 
                          (throw-error "ERROR" "what " :now "?")))))

(deftest curry-tests
  (testing "Currying."
    (let [f0 (->
               (fn [a b c d]
                 (/ (+ a (* b c) 1) d))
               (curry 4))]
      (is (= ((f0) 1 2 3 4) 2))
      (is (= ((f0 1) 2 3 4) 2))
      (is (= ((f0 1 2) 3 4) 2))
      (is (= ((f0 1 2 3) 4) 2))
      (is (= (f0 1 2 3 4) 2))

      (is (= (((f0 1) 2) 3 4) 2))
      (is (= ((((f0 1) 2) 3) 4) 2))
      (is (= (((f0 1 2) 3) 4) 2)))))
