(def nil '())
(def nil? (lambda (l) (= l nil)))
(def list (lambda () rest-args))
(def list? (lambda (l) (= "list" (typeof l))))

(def not (lambda (b) (= false b)))
(def and (lambda (a b) (cond (= true a b) true false)))
(def or (lambda (a b) (cond (= false a b) false true)))

(def defmacro (macro (name args body) (def name (macro args body))))

(def defn-core (lambda (name args body)
  (list def name
    (list lambda args
      (cond
        (nil? body) nil
        (nil? (cdr body)) (car body)
        (cons do body)
)))))
(defmacro defn (name args) (eval (defn-core 'name 'args 'rest-args)))

(defn fix (func) (func func))

(def fix-recursive-body (fix (lambda (self name body start?) (do
  (cond
    (nil? body) nil
    (list? (car body))
      (cons (self self name (car body) true) (self self name (cdr body) false))
    (and start? (= name (car body)))
      (cons name (cons name (self self name (cdr body) false)))
    (cons (car body) (self self name (cdr body) false))
)))))

(def defnrec-core (lambda (name args body)
  (list def name
    (list fix
      (list lambda args
        (cond
          (nil? body) nil
          (nil? (cdr body)) (car body)
          (cons do body)
))))))
(defmacro defnrec (name args) (eval (defnrec-core 'name (cons 'name 'args) (fix-recursive-body 'name 'rest-args true))))

(defnrec range (from to) (cond (> from to) nil (cons from (range (+ 1 from) to))))

(defnrec take (l cnt)
  (cond
    (<= cnt 0) nil
    (nil? l) nil
    (cons (car l) (take (cdr l) (- cnt 1)))
))

(defnrec drop (l cnt)
  (cond
    (<= cnt 0) l
    (nil? l) nil
    (drop (cdr l) (- cnt 1))
))

(defnrec length (l)
  (cond
    (nil? l) 0
    (+ 1 (length (cdr l)))
))

(defnrec contains? (elem l)
  (cond
    (nil? l) false
    (= elem (car l)) true
    (contains? elem (cdr l))
))

(defnrec iter (f l)
  (cond
    (nil? l) nil
    (do (f (car l)) (iter f (cdr l)))
))

(defnrec map (f l)
  (cond
    (nil? l) nil
    (cons (f (car l)) (map f (cdr l)))
))

(defnrec filter (f l)
  (cond
    (nil? l) nil
    (do
      (def rest (filter f (cdr l)))
      (cond
        (f (car l)) (cons (car l) rest)
        rest
))))

(defnrec fold-left (f acc l)
  (cond
    (nil? l) acc
    (fold-left f (f acc (car l)) (cdr l))
))
