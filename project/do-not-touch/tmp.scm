(remove-applic-lambda-nil
 '(applic
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
(applic
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
        5
        (applic (box-get (var fact)) ((const 5))))))
 ((const #f)))