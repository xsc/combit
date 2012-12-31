(ns ^{ :doc "Tests for Combit Combination Operator."
       :author "Yannick Scherer" }
  combit.combine-test
  (:use clojure.test
        combit.input-output
        combit.combine))

(deftest combination-tests
  (testing "Behaviour of Functions generated by `combine`"
    (let [in0 (input-data 0 4)
          in1 (input-data 1 2)
          out0 (output-data 0 4)
          out1 (output-data 1 2)
          cdta [7 8 9 10]
          dta [[1 2 3 4] [5 6]]
          odta [[0 0 0 0] [0 0]]
          c0 (const-data cdta)]
      (is (= ((combine (in0) (out0)) dta odta) [(first dta) (second odta)]))
      (is (= ((combine (c0) (out0)) dta odta) [cdta (second odta)]))
      (is (= ((combine (in1) (out1)) dta odta) [(first odta) (second dta)]))

      (are [ispec ospec r] (= ((combine (in0 ispec) (out0 ospec)) dta odta) [r (second odta)])
           0 0 [1 0 0 0]
           0 1 [0 1 0 0]
           0 2 [0 0 1 0]
           0 3 [0 0 0 1]
           1 0 [2 0 0 0]
           1 1 [0 2 0 0]
           1 2 [0 0 2 0]
           1 3 [0 0 0 2]
           2 0 [3 0 0 0]
           2 1 [0 3 0 0]
           2 2 [0 0 3 0]
           2 3 [0 0 0 3]
           3 0 [4 0 0 0]
           3 1 [0 4 0 0]
           3 2 [0 0 4 0]
           3 3 [0 0 0 4]
           [0 2] [1 3] [0 1 0 3]
           [0 2] [3 1] [0 3 0 1]
           [2 0] [1 3] [0 3 0 1]

           [1 2 3] [3 0 1] [3 4 0 2])
      (are [ispec ospec r] (= ((combine (in1 ispec) (out1 ospec)) dta odta) [(first odta) r])
           0 0 [5 0]
           0 1 [0 5]
           1 0 [6 0]
           1 1 [0 6]
           [0 1] [1 0] [6 5]
           [1 0] [0 1] [6 5]))))
