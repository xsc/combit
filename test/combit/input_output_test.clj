(ns ^{ :doc "Tests for Combit input/output/const Functions."
       :author "Yannick Scherer" }
  combit.input-output-test
  (:use clojure.test
        combit.input-output))

(deftest input-tests
  (testing "Behaviour of Functions generated by `input-data`"
    (let [dta [[1 2 3 4] [5 6]]
          in0 (input-data (first dta) 4)
          in1 (input-data (second dta) 2)]
      (is (= (in0) (first dta)))
      (is (= (in1) (second dta)))

      (are [spec r] (= (in0 spec) r)
           0 [1] 1 [2] 2 [3] 3 [4]
           [0 2] [1 3]
           [1 3] [2 4]
           [3 1] [4 2])
      (are [spec r] (= (in1 spec) r)
           0 [5] 1 [6]
           [0 1] [5 6]
           [1 0] [6 5])

      (are [l u r] (= (in0 l u) r)
           0 3 (first dta)
           3 0 (reverse (first dta))
           1 3 [2 3 4]
           2 1 [3 2])
      (are [l u r] (= (in1 l u) r)
           0 1 (second dta)
           1 0 (reverse (second dta))))))

(deftest output-tests
  (testing "Behaviour of Functions generated by `output-data`"
    (let [out0 (output-data 0 4)
          out1 (output-data 1 2)
          dta0 [[1 2 3 4]]
          dta1 [[5 6]]
          odta [[0 0 0 0] [0 0]]]
      (is (= ((out0) odta dta0) [(first dta0) (second odta)]))
      (is (= ((out1) odta dta1) [(first odta) (first dta1)]))

      (are [spec r] (= ((out0 spec) odta dta0) [r (second odta)])
           0 [1 0 0 0]
           1 [0 1 0 0]
           2 [0 0 1 0]
           3 [0 0 0 1]
           [0 2] [1 0 2 0]
           [1 3] [0 1 0 2]
           [3 1] [0 2 0 1])
      (are [spec r] (= ((out1 spec) odta dta1) [(first odta) r])
           0 [5 0]
           1 [0 5]
           [0 1] [5 6]
           [1 0] [6 5])

      (are [l u r] (= ((out0 l u) odta dta0) [r (second odta)])
           0 3 (first dta0)
           3 0 (reverse (first dta0))
           1 3 [0 1 2 3]
           2 1 [0 2 1 0])
      (are [l u r] (= ((out1 l u) odta dta1) [(first odta) r])
           0 1 (first dta1)
           1 0 (reverse (first dta1))))))
