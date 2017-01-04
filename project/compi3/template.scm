(define ^<template>
    (compose-patterns
     (pattern-rule
      `(var ,(? 'var))
      (lambda (var) `(var ,var)))

     (pattern-rule
      `(const ,(? 'const))
      (lambda (const) `(const ,const)))

     (pattern-rule
      `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
      (lambda (test dit dif) `(if3 ,(<rec-func> test) ,(<rec-func> dit) ,(<rec-func> dif))))

     (pattern-rule
      `(def ,(? 'var-name) ,(? 'val))
      (lambda (var-name val) `(def ,var-name ,(<rec-func> val))))

     (pattern-rule
      `(lambda-simple () ,(? 'body))
      (lambda (body) (<rec-func> body)))

     (pattern-rule
      `(lambda-simple ,(? 'args list?) ,(? 'body))
      (lambda (args body)
        `(lambda-simple ,args ,(<rec-func> body))))

     (pattern-rule
      `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
      (lambda (args opt-arg body) `(lambda-opt ,args ,opt-arg ,(<rec-func> body))))

     (pattern-rule
      `(lambda-var ,(? 'arg) ,(? 'body))
      (lambda (arg body) `(lambda-var ,arg ,(<rec-func> body))))

     (pattern-rule
      `(applic ,(? 'func) ,(? 'exprs list?))
      (lambda (func exprs) `(applic ,(<rec-func> func) ,(map <rec-func> exprs))))

     (pattern-rule
      `(or ,(? 'args list?))
      (lambda (args) `(or ,(map <rec-func> args))))

     (pattern-rule
      `(set ,(? 'var) ,(? 'val))
      (lambda (var val) `(set ,(<rec-func> var) ,(<rec-func> val))))

     (pattern-rule
      `(seq ,(? 'exprs list?))
      (lambda (exprs) `(seq ,(map <rec-func> exprs))))))
