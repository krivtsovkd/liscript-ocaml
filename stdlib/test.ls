(defn test (expected test-val) (cond (= expected test-val) () (panic (++ "Test failed: " expected " != " test-val))))

(defnrec fib (n) (cond (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(test '(1 2 3) (range 1 3))
(test '(7 8 9 10) (range 7 10))
(test nil (range 100 1))

(test '(1 2) (take (range 1 10) 2))
(test nil (take nil 1))
(test '("vasya" "petya" 1) (take '("vasya" "petya" 1 2 3) 3))
(test '(5 6 7 8 9) (take (range 5 9) 100))

(test 0 (length nil))
(test 3 (length (range 1 3)))
(test 2 (length (list "vasya" "petya")))

(test true (contains? 4 (range 1 10)))
(test false (contains? 10 (range 100 200)))
(test false (contains? "vasya" nil))
(test true (contains? "vasya" '("vasya" "petya" "vatya" "petya")))

(test '(3 4 5) (drop (range 1 5) 2))
(test nil (drop nil 1))
(test nil (drop (range 1 10) 100))

(test '(2 3 4) (map (lambda (n) (+ n 1)) (range 1 3)))
(test '(1 1 2 3 5 8 13 21 34 55) (map fib (range 1 10)))
(test '(true false true) (map not '(false true false)))

(test '(2 4) (filter (lambda (n) (= 0 (% n 2))) (range 1 5)))
(test nil (filter (lambda (n) (< n 0)) (range 100 200)))
(test '("vasya" "vasya") (filter (lambda (s) (= "vasya" s)) '("vasya" "pesya" "petya" "vasya")))

(test 55 (fold-left '+ 0 (range 1 10)))
(test 0 (fold-left '* 0 nil))
(test true (fold-left and true
  (map (lambda (n) (= 0 (% n 2))) (map (lambda (n) (* n 2)) (range 1 10)))
))
