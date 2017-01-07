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
                                        ; (annotate-tc (pe->lex-pe 
                                        ;(display (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines 
                                        ;(parse '(lambda (x  y) (lambda () y x (set! x 1 ))))))))))
(newline)
(load "hw3.so")
                                        ;(display (full-cycle (parse '(lambda (x  y) (lambda () y x (set! x 1 ))))))
#|(display (full-cycle (parse '(define read-stdin-to
      (lambda (end-of-input)
        (let ((end-of-input-list (string->list end-of-input)))
          (letrec ((state-init
                    (lambda (seen)
                      (let ((ch (read-char)))
                        (cond ((eof-object? ch)
                               (error 'read-stdin-to
                                      (format "Marker ~a not reached"
                                       end-of-input)))
                              ((char=? ch (car end-of-input-list))
                               (state-seen seen `(,ch) (cdr end-of-input-list)))
                              (else (state-init `(,ch ,@seen)))))))
                   (state-seen
                    (lambda (seen-before seen-now end-of-input-list-rest)
                      (if (null? end-of-input-list-rest)
                          (list->string
                           (reverse seen-before))
                          (let ((ch (read-char)))
                            (cond ((eof-object? ch)
                                   (format "Marker ~a not reached"
                                    end-of-input))
                                  ((char=? ch (car end-of-input-list-rest))
                                   (state-seen seen-before
                                    `(,ch ,@seen-now)
                                    (cdr end-of-input-list-rest)))
                                  (else (state-init
                                         `(,ch ,@seen-now ,@seen-before)))))))))
            (state-init '()))))))))|#


(display-colored-BIG 'full-cycle:)

(define my-full-cycle
  (lambda (e)
    (annotate-tc
     (pe->lex-pe
      (box-set
       (remove-applic-lambda-nil
        (eliminate-nested-defines
         (parse e))))))))

(let ((input
       '(define otherwise
          (lambda (p message)
            (lambda (s ret-match ret-none)
              (p
               s
               ret-match
               (let ((marker (format "-->[~a]" (list->string (list-head s *marker-length*)))))
                 (lambda (w)
                   (ret-none
                    (quasiquote ((unquote-splicing w) (unquote message) (unquote marker))))))))))))
  (ASSERT-EQUAL (my-full-cycle input) (full-cycle input)))


(let ((input '(define caten
                (letrec ((binary-caten
                          (lambda (p1 p2)
                            (lambda (s ret-match ret-none)
                              (p1
                               s
                               (lambda (e1 s)
                                 (p2
                                  s
                                  (lambda (e2 s)
                                    (ret-match (cons e1 e2) s)) ret-none)) ret-none))))
                         (loop
                          (lambda (ps)
                            (if (null? ps)
                                <epsilon>
                                (binary-caten
                                 (car ps)
                                 (loop (cdr ps)))))))
                  (lambda ps (loop ps))))))
  (display-colored-2 (parse input))
  (ASSERT-EQUAL (my-full-cycle input) (full-cycle input)))


'(def (var caten) 
  (applic 
   (lambda-simple 
    (binary-caten loop) 
    (seq ((set 
           (var binary-caten) 
           (lambda-simple (p1 p2) 
            (lambda-simple (s ret-match ret-none) 
             (applic (var p1) 
              ((var s) 
               (lambda-simple (e1 s) 
                (applic (var p2) 
                 ((var s) 
                  (lambda-simple (e2 s) 
                   (applic (var ret-match) 
                    ((applic 
                      (var cons) 
                      ((var e1) (var e2))) (var s)))) (var ret-none)))) (var ret-none)))))) 
          (set (var loop) 
           (lambda-simple (ps) 
            (if3 
             (applic 
              (var null?) 
              ((var ps))) 
             (var <epsilon>) 
             (applic 
              (var binary-caten) 
              ((applic (var car) 
                ((var ps))) 
               (applic (var loop) 
                ((applic (var cdr) ((var ps))))))))))
          (applic 
           (lambda-simple () 
            (lambda-var ps (applic (var loop) ((var ps))))) ())))
    ) ((const #f) (const #f))))

'(def (fvar otherwise)
  (lambda-simple (p message)
   (lambda-simple (s ret-match ret-none)
    (tc-applic
     (bvar p 0 0)
     ((pvar s 0)
      (pvar ret-match 1)
      (applic
       (lambda-simple (marker)
        (lambda-simple (w)
         (tc-applic
          (bvar ret-none 1 2)
          ((applic
            (fvar append)
            ((pvar w 0)
             (applic
              (fvar cons)
              ((bvar message 1 -1)
               (applic
                (fvar cons)
                ((bvar marker 0 0)
                 (const ())))))))))))
       ((applic
         (fvar format)
         ((const -->[~a])
          (applic (fvar list->string)
           ((applic
             (fvar list-head)
             ((pvar s 0)
              (fvar *marker-length*))))))))))))))

'(def (fvar const?)
  (applic
   (lambda-simple
    (simple-sexprs-predicates)
    (lambda-simple (e)
     (or ((applic (fvar ormap) ((lambda-simple (p?) (tc-applic (pvar p? 0) ((bvar e 0 0)))) (bvar simple-sexprs-predicates 0 0)))
          (tc-applic (fvar quote?) ((pvar e 0))))
         )))
   ((applic (fvar list) ((fvar boolean?) (fvar char?) (fvar number?) (fvar string?))))))


(display-colored-BIG (get-var-major-index 'message '((marker) (s ret-match ret-none) (p message) ())))

(newline)