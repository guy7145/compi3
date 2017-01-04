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


(display-colored-BIG 'nested-defs-tests:)
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





