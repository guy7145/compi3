(define <recurse-function>
  (let ((run (compose-patterns
              (pattern-rule
               `(var ,(? 'var))
               (lambda (var) `(var ,var)))

              (pattern-rule
               `(const ,(? 'const))
               (lambda (const) `(const ,const)))

              (pattern-rule
               `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
               (lambda (test dit dif) `(if3 ,(<recurse-function> test) ,(<recurse-function> dit) ,(<recurse-function> dif))))

              (pattern-rule
               `(def ,(? 'var-name) ,(? 'val))
               (lambda (var-name val) `(def ,var-name ,(<recurse-function> val))))

              (pattern-rule
               `(lambda-simple ,(? 'args list?) ,(? 'body))
               (lambda (args body)
                 `(lambda-simple ,args ,(<recurse-function> body))))

              (pattern-rule
               `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
               (lambda (args opt-arg body) `(lambda-opt ,args ,opt-arg ,(<recurse-function> body))))

              (pattern-rule
               `(lambda-var ,(? 'arg) ,(? 'body))
               (lambda (arg body) `(lambda-var ,arg ,(<recurse-function> body))))

              (pattern-rule
               `(applic ,(? 'func) ,(? 'exprs list?))
               (lambda (func exprs) `(applic ,(<recurse-function> func) ,(map <recurse-function> exprs))))

              (pattern-rule
               `(or ,(? 'args list?))
               (lambda (args) `(or ,(map <recurse-function> args))))

              (pattern-rule
               `(set ,(? 'var) ,(? 'val))
               (lambda (var val) `(set ,(<recurse-function> var) ,(<recurse-function> val))))

              (pattern-rule
               `(seq ,(? 'exprs list?))
               (lambda (exprs) `(seq ,(map <recurse-function> exprs))))

              (pattern-rule
               `(box ,(? 'var))
               (lambda (var) `(box ,(<recurse-function> var))))

              (pattern-rule
               `(box-get ,(? 'var))
               (lambda (var) `(box-get ,(<recurse-function> var))))

              (pattern-rule
               `(box-set ,(? 'var) ,(? 'val))
               (lambda (var val) `(box-set ,(<recurse-function> var) ,(<recurse-function> val))))
              )))
    
    (lambda (e)
      (run e (lambda () (error '<recurse-function> (format "I can't recognize this: ~s" e)))))))

