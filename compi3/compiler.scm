(load "pc.scm")

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser <end-of-input>)
              (*disj 2)
              done)))
    (new (*parser (char #\;))

         (*parser <any-char>)
         (*parser <end-of-line-comment>)
         *diff *star

         (*parser <end-of-line-comment>)
         (*caten 3)
         done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

;; ***********************************************
(define <delayed-infix-comment>
  (new (*delayed (lambda () <InfixComment>) ) done))
;; ***********************************************


(define <comment>
  (disj <line-comment>
        <delayed-infix-comment>
        <sexpr-comment>))

(define <skip>
  (disj <comment>
        <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
           (*parser <p>)
           (*parser <wrapper>)
           (*caten 3)
           (*pack-with
            (lambda (_left e _right) e))
           done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;;;;;;;; END OF SKIPPED WHITE SPACE ;;;;;;;;;;;;;;

(define <Boolean>
  (new

   (*parser (char-ci #\#))
   (*parser (char-ci #\f))
   (*parser (char-ci #\t))

   (*disj 2)
   (*caten 2)

   (*pack-with
    (lambda (_ t)
      (eq? t #\t)))

   done))

(define <CharPrefix>
  (new

   (*parser (char #\#))
   (*parser (char #\\))
   (*caten 2)

   done))

(define <VisibleSimpleChar>
  (new

   (*parser (const
             (lambda (ch)
               (char<? #\space ch))))

   done))

(define <NamedChar>
  (new

   (*parser (word-ci "lambda"))
   (*parser (word-ci "newline"))
   (*parser (word-ci "nul"))
   (*parser (word-ci "page"))
   (*parser (word-ci "return"))
   (*parser (word-ci "space"))
   (*parser (word-ci "tab"))

   (*disj 7)

   (*pack (lambda (x)
            (let ((x (list->string x)))
              (cond ((string-ci=? x "lambda")  #\x3bb)
                    ((string-ci=? x "newline") #\newline)
                    ((string-ci=? x "nul")     #\nul)
                    ((string-ci=? x "page")    #\page)
                    ((string-ci=? x "return")  #\return)
                    ((string-ci=? x "space")   #\space)
                    ((string-ci=? x "tab")     #\tab)
                    ))))

   done))

(define <HexChar>
  (let ((zero (char->integer #\0))
        (lc-a (char->integer #\a))
        (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
         (*pack
          (lambda (ch)
            (- (char->integer ch) zero)))

         (*parser (range #\a #\f))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) lc-a))))

         (*parser (range #\A #\F))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) uc-a))))

         (*disj 3)
         done)))

(define <HexUnicodeChar>
  (letrec ((hex->int (lambda (lst c)
                       (if (null? lst)
                           c
                           (hex->int (cdr lst) (+ (car lst) (* 16 c))))
                       )))
    (new
     (*parser (char #\x))
     (*parser <HexChar>) *plus
     (*caten 2)

     (*pack-with
      (lambda (x hc)
        (integer->char (hex->int `(,@hc) 0))))

     done)))

(define <Char>
  (new
   (*parser <CharPrefix>)
   (*parser <NamedChar>)
   (*parser <HexUnicodeChar>)
   (*parser <VisibleSimpleChar>)

   (*disj 3)
   (*caten 2)

   (*pack-with
    (lambda (p cs)
      cs;(string->symbol (string cs))
      ))

   done))

(define <Natural>
  (let ((char->int (lambda (c)
                     (- (char->integer c)
                        (char->integer #\0)))))
    (new
     (*parser (range #\0 #\9)) *plus
     (*pack
      (lambda (lst)
        (letrec ((lst->int (lambda (lst c)
                             (if (null? lst)
                                 c
                                 (lst->int (cdr lst) (+ (* c 10) (char->int (car lst))))))))
          (lst->int lst 0))))
     done)))

(define <Integer>
  (new
   (*parser (char #\+))
   (*parser (char #\-))
   (*disj 2) *maybe
   (*parser <Natural>)
   (*caten 2)
   (*pack-with
    (lambda (s n)
      (if (and (car s) (equal? (cadr s) #\-))
          (- n)
          n)))
   done))

(define <Fraction>
  (new
   (*parser <Integer>)
   (*parser (char #\/))
   (*parser <Natural>)
   (*caten 3)
   (*pack-with
    (lambda (int _ nat)
      (/ int nat)))
   done))

(define <Number>
  (new
   (*parser <Fraction>)
   (*parser <Integer>)
   (*disj 2)
   (*delayed (lambda () <SymbolChar>))
   (*parser (range #\0 #\9))
   *diff
   *not-followed-by
   done))

(define <StringVisibleChar>
  (new
   (*parser (const
             (lambda (ch)
               (char<=? #\space ch))))
   done))

(define <StringMetaChar>
  (new
   (*parser (word-ci "\\\\"))
   (*parser (word-ci "\\\""))
   (*parser (word-ci "\\t"))
   (*parser (word-ci "\\f"))
   (*parser (word-ci "\\n"))
   (*parser (word-ci "\\r"))
   (*disj 6)
   (*pack (lambda (x)
            (let ((c (cadr x)))
              (cond ((char-ci=? c #\t) #\tab)
                    ((char-ci=? c #\f) #\x0c)
                    ((char-ci=? c #\n) #\newline)
                    ((char-ci=? c #\r) #\return)
                    (else c)))))
   done))

(define <StringHexChar>
  (letrec ((hex->int (lambda (lst c)
                       (if (null? lst)
                           c
                           (hex->int (cdr lst) (+ (car lst) (* 16 c))))
                       )))
    (new
     (*parser (word-ci "\\x"))
     (*parser <HexChar>) *star
     (*parser (char #\;))
     (*caten 3)
     (*pack-with
      (lambda (x lst _)
        (integer->char (hex->int lst 0))))
     done)))

(define <StringChar>
  (new
   (*parser <StringHexChar>)
   (*parser <StringMetaChar>)
   (*parser <StringVisibleChar>)
   (*disj 3)
   done))

(define <String>
  (new
   (*parser (char #\"))
   (*parser <StringChar>)
   (*parser (char #\"))
   *diff
   *star
   (*parser (char #\"))
   (*caten 3)
   (*pack-with
    (lambda (< lst >)
      (list->string lst)))
   done))

(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range #\a #\z))
   (*parser (range #\A #\Z))
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\-))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
   (*disj 15)
   (*pack char-downcase)
   done))

(define <Symbol>
  (new
   (*parser <SymbolChar>) *plus
   (*pack (lambda (lst)
            (string->symbol
             (list->string lst))))
   done))

(define <ProperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>)) *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (_ lst __) lst))
   done))

(define <ImproperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>)) *plus
   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))
   (*caten 5)
   (*pack-with
    (lambda (_ lst __ itm ___)
      (append lst itm)))
   done))

(define <Vector>
  (new
   (*parser (word "#("))
   (*delayed (lambda () <sexpr>)) *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (_ lst __)
      (list->vector lst)))
   done))

(define <Quoted>
  (new
   (*parser (char #\'))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'quote exp)))
   done))

(define <QuasiQuoted>
  (new
   (*parser (char #\`))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'quasiquote exp)))
   done))

(define <Unquoted>
  (new
   (*parser (char #\,))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'unquote exp)))
   done))

(define <UnquoteAndSpliced>
  (new
   (*parser (word ",@"))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'unquote-splicing exp)))
   done))

                                        ; ************************************************************************************************************************************************************************************


(define *<InfixExpressionDelayed>
  (*delayed (lambda () <InfixExpression>)))

(define <InfixPrefixExtensionPrefix>
  (^<skipped*>
   (new
    (*parser (char #\#))
    (*parser (char #\#))
    (*caten 2)
    (*parser (char #\#))
    (*parser (char #\%))
    (*caten 2)
    (*disj 2)
    (*pack-with
     (lambda (x1 x2) (list->string `(,x1 (,@x2)))))
    done)))

(define <InfixExtension>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
   *<InfixExpressionDelayed>
   (*caten 2)
   (*pack-with
    (lambda (_ expr) expr))
   done))

(define <PowerSymbol>
  (new
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\*))
   (*caten 2)
   (*pack-with
    (lambda (x1 x2) (list->string `(,x1 (,@x2)))))
   (*disj 2)
   done))

;; Auxilary Lambdas
;; 

; for backward compatibility (;
(define build-op-formula
  (lambda (<op>)
    (lambda (l r)
      `(,<op> ,l ,r))))

(define <operator-2ops>
  (lambda (<folding> <formula-builder>)
    (lambda (<op-char-parser> <fold-operation> <next-parser>)
      (new
       (*parser <next-parser>)
       (*parser (^<skipped*> <op-char-parser>))
       (*caten 2)
       (*pack-with
        (lambda (expr op-char) expr))
       *plus
       (*parser <next-parser>)
       (*caten 2)
       (*pack-with
        (let ((formula-builder (<formula-builder> <fold-operation>)))
          (lambda (list element)
            (<folding> formula-builder element list))))
       (*parser <next-parser>)
       (*disj 2)
       done))))

(define <operator-2ops-right-lr> (<operator-2ops> fold-right build-op-formula))

;; Level
;;
(define <InfixSymbol>
  (new
   (*parser <SymbolChar>)
   (*parser (char #\+))
   (*parser (char #\-))
   (*parser (char #\*))
   (*parser (char #\/))
   (*parser <PowerSymbol>)
   (*disj 5)
   *diff
   *plus
   (*pack (lambda (lst)
            (string->symbol
             (list->string lst))))
   done))

(define <InfixSexprEscape>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (prefix sexpr) sexpr))
   done))

(define <infix-number> (not-followed-by (disj <Fraction> <Integer>) <InfixSymbol>))

(define <InfixParen>
  (new
   (*parser (char #\())
   *<InfixExpressionDelayed>
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (lp expr rp) expr))
   done))

(define <Primitives-and-such> (disj <infix-number> <InfixSexprEscape> <InfixSymbol>))
(define <Level-Paren-&-Friends> (disj <InfixParen> <Primitives-and-such>))

;; Level
;;
(define <InfixArgList>
  (^<skipped*> (new
                ;; args ;;
                ; first arg
                *<InfixExpressionDelayed>
                                        ; rest args (some or none)
                (*parser (char #\,))
                *<InfixExpressionDelayed>
                (*caten 2)
                (*pack-with (lambda (_ expr) expr))
                *star
                                        ; catenate
                (*caten 2)
                (*pack-with
                 (lambda (first-arg rest-args) `(,first-arg ,@rest-args)))
                ;; or no args ;;
                (*parser <epsilon>)
                ;; args | no args ;;
                (*disj 2)
                done)))

(define <InfixFuncall>
  (new
   (*parser (^<skipped*> <Level-Paren-&-Friends>))
   (*parser (char #\())
   (*parser <InfixArgList>)
   (*parser (char #\)))
   (*caten 4)
   (*pack-with
    (lambda (function lp args rp)
      `(,function ,@args)))
   done))

(define <InfixArrayGet> (^<skipped*>
                         (new
                          (*parser (^<skipped*> <Level-Paren-&-Friends>))
                          (*parser (char #\[))
                          (*delayed (lambda () <InfixArrayGet>))
                          (*delayed (lambda () <InfixExpression>))
                          (*disj 2)
                          (*parser (char #\]))
                          (*caten 3)
                          (*pack-with
                           (lambda (lp expr rp) expr))
                          *plus
                          (*caten 2)
                          (*pack-with
                           (let ((expr-builder (build-op-formula 'vector-ref)))
                             (lambda (arr is)
                               (fold-left expr-builder arr is))))
                          done)))

(define <Level-ArrFun> (disj <InfixArrayGet> <InfixFuncall> <Level-Paren-&-Friends>))

;; Level
;;

(define <InfixNeg>
  (new (*parser (let ((<next-level> (new (*delayed (lambda () <InfixNeg>))
                                         (*parser <Level-ArrFun>)
                                         (*disj 2)
                                         done)))
                  (new
                   ;; (without parentesis) 
                   ;; no spaces between '-' and the following expression 
                   (*parser (char #\-))
                   (*parser <next-level>)
                   (*caten 2)
                   (*pack-with
                    (lambda (minus element)
                      (cond ((number? element) (- element))
                            (else `(- ,element)))))
                   ;; (with parentesis) 
                   ;; spaces between '-' and the following expression
                   (*parser (^<skipped*> (char #\-)))
                   (*parser <next-level>)
                   (*caten 2)
                   (*pack-with
                    (lambda (minus element) `(- ,element)))
                   (*disj 2)
                   done)))
       (*parser <Level-ArrFun>)
       (*disj 2)
       done))

;; Level
;; 
(define <InfixPow> (<operator-2ops-right-lr> <PowerSymbol> (string->symbol "expt") <InfixNeg>))

;; Level
;; 
(define build-op-formula-op1-op2
  (lambda (element op-expr-pair)
    (let ((op (car op-expr-pair)) (expr (cdr op-expr-pair)))
      `(,op ,element ,@expr))))

(define <level-op1-op2>
  (lambda (<next-level> op-char1 op-char2 op-str1 op-str2)
    (new (*parser <next-level>)
         (*parser (^<skipped*> (char op-char1)))
         (*parser (^<skipped*> (char op-char2)))
         (*disj 2)
         (*pack (lambda (op) (cond ((char=? op op-char1) (string->symbol op-str1))
                                   (else (string->symbol op-str2)))))
         (*parser <next-level>)
         (*caten 2)
         *plus
         (*caten 2)
         (*pack-with
          (lambda (element list)
            (fold-left build-op-formula-op1-op2 element list)))
         (*parser <next-level>)
         (*disj 2)
         done)))

;; Level
;; 
(define <Level-MulDiv> (<level-op1-op2> <InfixPow> #\/ #\* "/" "*"))

;; Level
;;
(define <Level-AddSub> (<level-op1-op2> <Level-MulDiv> #\+ #\- "+" "-"))

;; Entry Point
;; 
(define <InfixExpression> (^<skipped*> <Level-AddSub>))

;; Comments
;; 
(define <InfixComment>
  (new (*parser (word "#;"))
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (_1 _2) (void)))
       done))

                                        ; ************************************************************************************************************************************************************************************

(define <sexpr>
  (^<skipped*>
   (disj <Boolean>
         <InfixComment>
         <InfixExtension>
         <Char>
         <String>
         <Number>
         <Symbol>
         <ProperList>
         <ImproperList>
         <Vector>
         <Quoted>
         <QuasiQuoted>
         <Unquoted>
         <UnquoteAndSpliced>
         )))

;*************************************************************************************************

(load "pattern-matcher.scm")

#| .:: tools rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define *void-object* (void))

(define *reserved-words*
  '(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!))

(define *error-text* "ERROR")

(define *error-continuation*
  (lambda () *error-text*))

(define reserved-word?
  (lambda (x)
    (member x *reserved-words*)))

(define not-reserved-word?
  (lambda (x)
    (not (member x *reserved-words*))))

(define var?
  (lambda (x)
    (and (symbol? x)
         (not-reserved-word? x))))

(define simple-const?
  (let ((preds (list boolean? char? number? string?)))
    (lambda (e)
      (ormap (lambda (p?) (p? e)) preds))))

(define listify
  (lambda (x)
    (cond ((null? x) '())
          ((pair? x) x)
          (else `(,x)))))

(define list-is-duplicative?
  (lambda (s)
    (cond ((null? s) #f)
          ((member (car s) (cdr s)) #t)
          (else (list-is-duplicative? (cdr s))))))

(define beginify
  (lambda (s)
    (cond ((null? s) *void-object*)
          ((null? (cdr s)) (car s))
          (else `(begin ,@s)))))

#| .:: basic rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <void-rule>
  (pattern-rule
   (void)
   (lambda () `(const ,*void-object*))))

(define <const-rule>
  (pattern-rule
   (? 'c simple-const?)
   (lambda (c) `(const ,c))))

(define <quote-rule>
  (pattern-rule
   `(quote ,(? 'c))
   (lambda (c) `(const ,c))))

(define <var-rule>
  (pattern-rule
   (? 'var var?)
   (lambda (var) `(var ,var))))

#| .:: assignment rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <assignment-rule>
  (pattern-rule
   `(set! ,(? 'var var?) ,(? 'val))
   (lambda (var val)
     `(set ,(parse var) ,(parse val)))))

#| .:: application rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <application-rule>
  (pattern-rule
   `(,(? 'foo not-reserved-word?) . ,(? 'args))
   (lambda (foo . args)
     `(applic ,(parse foo) (,@(map parse (car args)))))))


#| .:: if rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <if2-rule>
  (pattern-rule
   `(if ,(? 'test) ,(? 'dit))
   (lambda (test dit)
     `(if3 ,(parse test) ,(parse dit) (const ,*void-object*)))))

(define <if3-rule>
  (pattern-rule
   `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
   (lambda (test dit dif)
     `(if3 ,(parse test) ,(parse dit) ,(parse dif)))))

#| .:: disjunction rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <disj-rule-no-args>
  (pattern-rule
   `(or)
   (lambda () `(const ,#f))))

(define <disj-rule-single-arg>
  (pattern-rule
   `(or ,(? 'expr))
   (lambda (expr) (parse expr) )))

(define <disj-rule-several-args>
  (pattern-rule
   `(or ,(? 'expr) . ,(? 'rest-exprs))
   (lambda (expr . rest-exprs)
     (let ((rest-exprs-unwrapped (car rest-exprs)))
       `(or (,(parse expr) ,@(map parse rest-exprs-unwrapped)))))))

(define <disj-rule>
  (compose-patterns
   <disj-rule-no-args>
   <disj-rule-single-arg>
   <disj-rule-several-args>
   ))

#| .:: and rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <and-rule-no-args>
  (pattern-rule
   `(and)
   (lambda () `(const ,#t))))

(define <and-rule-with-args>
  (pattern-rule
   `(and ,(? 'expr) . ,(? 'rest-exprs))
   (lambda (expr . rest-exprs)
     (letrec ((rest-exprs-unwrapped (car rest-exprs))
              (and->if (lambda (lst)
                         (if (null? (cdr lst))
                             (car lst)
                             (list 'if
                                   (car lst)
                                   (and->if (cdr lst))
                                   #f)))))
       (parse (and->if `(,expr ,@(car rest-exprs))))))))

(define <and-rule>
  (compose-patterns
   <and-rule-no-args>
   <and-rule-with-args>))

#| .:: cond rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <cond-rule>
  (pattern-rule
   `(cond ,(? 'expr) . ,(? 'exprs))
   (lambda (head tail)
     (letrec ((cond->if (lambda (lst)
			  (if (null? lst)
			      (void)
			      (if (equal? 'else (caar lst))
				  (beginify (cdar lst))
				  `(if ,(caar lst)
				       ,(beginify (cdar lst))
				       ,(cond->if (cdr lst))))))))
       (parse (cond->if `(,head ,@tail)))))))

#| .:: define rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <define-rule>
  (pattern-rule
   `(define ,(? 'var) ,(? 'val) . ,(? 'val-rest))
   (lambda (var val . val-rest)
     (if (null? val-rest)
         `(def ,(parse var) ,(parse val))
         `(def ,(parse var) ,(parse (beginify (cons val (car val-rest)))))))))

(define merge-bodies
  (lambda (body rest-body)
    (if (null? rest-body)
        body
        (beginify (cons body (car rest-body))))))

(define <define-mit-rule-var>
  (pattern-rule 
   `(define (,(? 'object) . ,(? 'var-arg)) ,(? 'body) . ,(? 'rest-body))
   (lambda (object var-arg body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       `(def ,(parse object) ,(parse `(lambda ,var-arg ,body)))
   ))))

(define <define-mit-rule-simple-opt>
  (pattern-rule
   `(define (,(? 'object) ,(? 'args)) ,(? 'body))
   (lambda (object args body . rest-body) 
       `(def ,(parse object) ,(parse `(lambda ,args ,body))))))

(define <define-mit-rule>
  (compose-patterns
   <define-mit-rule-var>
   <define-mit-rule-simple-opt>
   ))


#| .:: begin rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define get-tag car)
(define get-data cdr)

(define <begin-rule-empty>
  (pattern-rule
   `(begin)
   (lambda () `(const ,*void-object*))))

(define <begin-rule-single-statement>
  (pattern-rule
   `(begin ,(? 'body))
   (lambda (body) (parse body))))

(define flatten-list
  (lambda (s)
    (cond ((null? s) '())
          ((list? (car s)) (append (car s) (flatten-list (cdr s))))
          (else (cons (car s) (flatten-list (cdr s)))))))

(define <begin-rule-several-statements>
  (let ((parse-unwrap
         (lambda (e)
           (let ((e-tagged (parse e)))
             (if (equal? 'seq (get-tag e-tagged)) (cadr e-tagged) (list e-tagged))))))
    (pattern-rule
     `(begin ,(? 'first-statement) . ,(? 'rest-statements))
     (lambda (first-statement . rest-statements)
       (let ((body (cons first-statement (car rest-statements))))
         `(seq ,(flatten-list (map parse-unwrap body))))))))

(define <seq-rule-explicit>
  (compose-patterns
   <begin-rule-empty>
   <begin-rule-single-statement>
   <begin-rule-several-statements>
   ))

#| .:: lambda rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define identify-lambda
  (lambda (args ret-simple ret-opt ret-var)
    (cond ((null? args) (ret-simple '()))
          ((symbol? args) (ret-var args))
          (else (identify-lambda
                 (cdr args)
                 (lambda (s) (ret-simple `(,(car args) ,@s))) ;simple
                 (lambda (s . opt) (ret-opt `(,(car args) ,@s) (car opt))) ; opt
                 (lambda (var) (ret-opt `(,(car args)) `(,var)))) ; var
           ))))

(define args-not-duplicative?
  (lambda (args)
    (not (and (list? args) (list-is-duplicative? args)))))

(define <lambda-rule>
  (pattern-rule
   `(lambda ,(? 'args args-not-duplicative?) ,(? 'body) . ,(? 'rest-body))
   (lambda (args body . rest-body)
     (let ((rest-body (car rest-body)))

       (let ((body (if (null? rest-body)
                       body
                       (beginify (cons body rest-body)))))

         (let ((parsed-body (parse body)))

           (identify-lambda
            args
            (lambda (s) `(lambda-simple ,s ,parsed-body)) ; simple
            (lambda (s opt) `(lambda-opt ,s ,@opt ,parsed-body)) ; opt
            (lambda (var) `(lambda-var ,var ,parsed-body)) ; var
            )))))))

#| .:: let rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <let-no-args-rule>
  (pattern-rule
   `(let () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ,body)))))))

(define <let-rule>
  (lambda (e fail-cont)
    ((pattern-rule
   `(let (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
   (lambda (args-head rest body . rest-body)
     (let ((args (if (null? rest)
		     `(,args-head)
		     `(,args-head ,@rest)))
	   (body (if (null? rest-body)
		     `(,body)
		     `(,body ,@(car rest-body)))))
       (if (list-is-duplicative? (map car args))
	   (fail-cont)
	   (parse `((lambda ,(map car args) ,@body) ,@(map cadr args))))))) e fail-cont)))

(define <let*-no-args-rule>
  (pattern-rule
   `(let* () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ,body)))))))

(define <let*-rule>
  (pattern-rule
   `(let* (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
   (lambda (args-head rest head-body . rest-body)
     (letrec ((args (if (null? rest)
			`(,args-head)
			`(,args-head ,@rest)))
	      (body (if (null? rest-body)
			`(,head-body)
			`(,head-body ,@(car rest-body))))
	      (let*->encapsulated-lambdas (lambda (args body)
			       (if (null? args)
				   body
				   `(((lambda (,(caar args))
					,@(let*->encapsulated-lambdas (cdr args) body))
				      ,@(cdar args)))))))
       (parse (car (let*->encapsulated-lambdas args body)))))))

(define <letrec-no-args-rule>
  (pattern-rule
   `(letrec () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ((lambda () ,body)))))))))

(define <letrec-rule>
  (lambda (e fail-cont)
    ((pattern-rule
   `(letrec (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
   (lambda (args-head rest head-body . rest-body)
     (letrec ((args (if (null? rest)
			`(,args-head)
			`(,args-head ,@rest)))
	      (body (if (null? rest-body)
			`(,head-body)
			`(,head-body ,@(car rest-body))))
	      (args->set (lambda (lst)
			   (if (null? (cdr lst))
			       `((set! ,(caar lst) ,@(cdar lst)))
			       `((set! ,(caar lst) ,@(cdar lst)) ,@(args->set (cdr lst)))))))
       (if (list-is-duplicative? (map car args))
	   (fail-cont)
	   (parse `((lambda ,(map car args) ,@(append (args->set args) `(((lambda () ,@body))))) ,@(map (lambda (x) #f) args))))))) e fail-cont)))

(define <let-rules>
  (compose-patterns
   <let-no-args-rule>
   <let-rule>
   <let*-no-args-rule>
   <let*-rule>
   <letrec-no-args-rule>
   <letrec-rule>
   ))


#| .:: qq rule ::. ______________________________________________________________________________________________________________________________________________________|#

;;; qq.scm
;;; A naive, one-level quasiquote implementation + optimizations
;;;
;;; Programmer: Mayer Goldberg, 2016



;;;

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))
#|
(define <qq-rule>
  (pattern-rule
   `(quasiquote ,(? 'd))
   (lambda (d) d)))|#

(define <qq-rule>
  (pattern-rule
   (? 'qq (lambda (x) (list? x)) (lambda (x)
                                   (and (equal? 'quasiquote (get-tag x))
                                        (not (equal? '() (get-data x))))))
   (lambda (c) (parse (expand-qq (cadr c))))))

#| .:: PARSING INTERFACE ::. ______________________________________________________________________________________________________________________________________________________|#

(define tag-parse
  (let ((run
         (compose-patterns
          <qq-rule>
          
          <void-rule>
          <const-rule>
          <quote-rule>
          <var-rule>
          
          <if2-rule>
          <if3-rule>
          
          <define-mit-rule>
          <define-rule>
          
          <disj-rule>
          
          <lambda-rule>
          
          <seq-rule-explicit>
          
          <assignment-rule>
          <application-rule>
          
          <and-rule-no-args>
          <and-rule>
          <cond-rule>
          
          <let-rules>
          )
         ))
    (lambda (sexpr)
      (run sexpr *error-continuation*))))

(define parse tag-parse)
