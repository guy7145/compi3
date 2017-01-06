(load "tdd-tools.scm")
(load "compiler.scm")

(display-colored-BIG 'remove-applic-lambda-nil:)
(let ((input '(applic
               (lambda-simple
                (fact)
                (seq ((set (var fact) (box (var fact)))
                      (box-set
                       (var fact)
                       (lambda-simple
                        (n)
                        (if3 (applic (var zero?) ((var n)))
                         (const 1)
                         (applic
                          (var *)
                          ((var n)
                           (applic
                            (box-get (var fact))
                            ((applic (var -) ((var n) (const 1))))))))))
                      (applic
                       (lambda-simple () (applic (box-get (var fact)) ((const 5))))
                       ()))))
               ((const #f))))
      (output '(applic
                (lambda-simple
                 (fact)
                 (seq ((set (var fact) (box (var fact)))
                       (box-set
                        (var fact)
                        (lambda-simple
                         (n)
                         (if3 (applic (var zero?) ((var n)))
                          (const 1)
                          (applic
                           (var *)
                           ((var n)
                            (applic
                             (box-get (var fact))
                             ((applic (var -) ((var n) (const 1))))))))))
                       (applic (box-get (var fact)) ((const 5))))))
                ((const #f)))))
  (ASSERT-EQUAL (remove-applic-lambda-nil input) output))


(display-colored-BIG 'eliminate-nested-defines:)
(define nested-defs-tests
  (list
   (cons
    '(define my-even?
       (lambda (e)
         (define even?
           (lambda (n)
             (or (zero? n)
                 (odd? (- n 1)))))
         (define odd?
           (lambda (n)
             (and (positive? n)
                  (even? (- n 1)))))
         (even? e)))
    '(def (var my-even?)
      (lambda-simple
       (e)
       (applic
        (lambda-simple
         (even? odd?)
         (seq ((set (var even?)
                (lambda-simple
                 (n)
                 (or ((applic (var zero?) ((var n)))
                      (applic
                       (var odd?)
                       ((applic (var -) ((var n) (const 1)))))))))
               (set (var odd?)
                (lambda-simple
                 (n)
                 (if3 (applic (var positive?) ((var n)))
                  (applic
                   (var even?)
                   ((applic (var -) ((var n) (const 1)))))
                  (const #f))))
               (applic (var even?) ((var e))))))
        ((const #f) (const #f)))))
    )
   ))

(define RUN-TEST
  (lambda (f tests)
    (let ((tmp (map
                (lambda (test)
                  (ASSERT-EQUAL (f (parse (car test))) (cdr test)))
                tests)))
      #t)))

(RUN-TEST eliminate-nested-defines nested-defs-tests)


(display-colored-BIG 'pe->lex-pe:)
(let ((input  '(x (lambda (x) (x (lambda () (x (lambda () (x x))))))))
      (output '(applic
                (fvar x)
                ((lambda-simple
                  (x)
                  (applic
                   (pvar x 0)
                   ((lambda-simple
                     ()
                     (applic
                      (bvar x 0 0)
                      ((lambda-simple
                        ()
                        (applic (bvar x 1 0) ((bvar x 1 0))))))))))))))
  (ASSERT-EQUAL (pe->lex-pe (parse input)) output))

(let ((input '(lambda (a b) (lambda (c) (+ a b c))))
      (output '(lambda-simple
                (a b)
                (lambda-simple
                 (c)
                 (applic (fvar +) ((bvar a 0 0) (bvar b 0 1) (pvar c 0)))))))
  (ASSERT-EQUAL (pe->lex-pe (parse input)) output))

(let ((input '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))))
      (output '(def (fvar fact)
                (lambda-simple
                 (n)
                 (if3 (applic (fvar zero?) ((pvar n 0)))
                  (const 1)
                  (applic
                   (fvar *)
                   ((pvar n 0)
                    (applic
                     (fvar fact)
                     ((applic (fvar -) ((pvar n 0) (const 1))))))))))))
  (ASSERT-EQUAL (pe->lex-pe (parse input)) output))


(display-colored-BIG 'annotate-tc:)
(let ((input '(lambda (x) (x x)))
      (output '(lambda-simple (x) (tc-applic (var x) ((var x))))))
  (ASSERT-EQUAL (annotate-tc (parse input)) output))

(let ((input '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))))
      (output '(def (var fact)
                (lambda-simple
                 (n)
                 (if3 (applic (var zero?) ((var n)))
                  (const 1)
                  (tc-applic
                   (var *)
                   ((var n)
                    (applic
                     (var fact)
                     ((applic (var -) ((var n) (const 1))))))))))))
  (ASSERT-EQUAL (annotate-tc (parse input)) output))

(let ((input '(x (lambda (x) (x (lambda () (x (lambda () (x x))))))))
      (output '(applic
                (var x)
                ((lambda-simple
                  (x)
                  (tc-applic
                   (var x)
                   ((lambda-simple
                     ()
                     (tc-applic
                      (var x)
                      ((lambda-simple () (tc-applic (var x) ((var x))))))))))))))
  (ASSERT-EQUAL (annotate-tc (parse input)) output))

(let ((input '(lambda (f)
                ((lambda (x) (f (lambda s (apply (x x) s))))
                 (lambda (x) (f (lambda s (apply (x x) s)))))))
      (output '(lambda-simple
                (f)
                (tc-applic
                 (lambda-simple
                  (x)
                  (tc-applic
                   (var f)
                   ((lambda-var
                     s
                     (tc-applic
                      (var apply)
                      ((applic (var x) ((var x))) (var s)))))))
                 ((lambda-simple
                   (x)
                   (tc-applic
                    (var f)
                    ((lambda-var
                      s
                      (tc-applic
                       (var apply)
                       ((applic (var x) ((var x))) (var s))))))))))))
  (ASSERT-EQUAL (annotate-tc (parse input)) output))


(display-colored-BIG 'box-set:)
(let ((input '(let ((a 0))
                (list
                 (lambda () a)
                 (lambda () (set! a (+ a 1)))
                 (lambda (b) (set! a b)))))
      (output '(applic
                (lambda-simple
                 (a)
                 (seq ((set (var a) (box (var a)))
                       (applic
                        (var list)
                        ((lambda-simple () (box-get (var a)))
                         (lambda-simple
                          ()
                          (box-set
                           (var a)
                           (applic (var +) ((box-get (var a)) (const 1)))))
                         (lambda-simple (b) (box-set (var a) (var b))))))))
                ((const 0)))))
  (ASSERT-EQUAL (box-set (eliminate-nested-defines (parse input))) output))

; (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse input))))))

(display (get-var-minor-index 'c '(a b c)))

(newline)