(defn test (expected test-val) (cond (= expected test-val) () (panic (++ "Test failed: " expected " != " test-val))))

(test '(1 2 3) (range 1 3))
(test nil (range 100 1))
