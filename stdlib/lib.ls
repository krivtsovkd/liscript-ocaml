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
