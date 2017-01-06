(load "myparser.scm") 0

(define exract_def
	(lambda (pes ret-ds-es)
	(if (null? pes) (ret-ds-es '() '())
	(exract_def
		(cdr pes)
		(lambda (ds es)
			(cond 	((eq? (caar pes) 'def) (ret-ds-es (cons (car pes) ds) es))
					((eq? (caar pes) 'seq)
						(exract_def (cadar pes)
						(lambda (ds1 es1)
							(ret-ds-es (append ds1 ds)
							(append es1 es)))))
			(else (ret-ds-es ds (cons (car pes) es)))))))))



(define eliminate-nested-defines
		(let ((run
				(compose-patterns
					(pattern-rule
					`(def ,(? 'var) ,(? 'val))
					(lambda (var val) `(def ,var ,(eliminate-nested-defines val))))
						
					(pattern-rule
					`(seq ,(? 'vars list?))
					(lambda (vars) `(seq ,(map eliminate-nested-defines vars))))
										
					
					(pattern-rule 
					`(var ,(? 'var))
					(lambda(var) `(var ,var)))
					
					(pattern-rule 
					`(const ,(? 'const))
					(lambda(const) `(const ,const)))
					
					(pattern-rule 
					`(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda(test dit dif) `(if3 ,(eliminate-nested-defines test) ,(eliminate-nested-defines dit) ,(eliminate-nested-defines dif))))
					
					(pattern-rule 
					`(lambda-simple ,(? 'args list?) ,(? 'exprs))
					(lambda(args exprs) 
						(exract_def `(,(eliminate-nested-defines exprs)) 
							(lambda (ds es)
									(if (null? ds) (if (=(length es) 1) `(lambda-simple ,args ,(car es)) `(lambda-simple ,args (seq ,es)))
										
										(let ((params (map (lambda (def-st) (car (cdr (car (cdr def-st))))) ds))
											  (false-vals  (map (lambda (def-set) `(const #f)) ds))
											  (body `(seq ,(append (map (lambda (def-st) `(set ,(car (cdr def-st)) ,(car (cdr (cdr def-st))))) ds) es))))
											`(lambda-simple ,args (applic (lambda-simple ,params ,body) ,false-vals))
											)
										)
									
									
									))
					))
					
					(pattern-rule 
					`(lambda-opt ,(? 'args list?) ,(? 'rest) ,(? 'exprs))
					(lambda(args rest exprs) `(lambda-opt ,args ,rest ,(eliminate-nested-defines exprs))))
					
					(pattern-rule 
					`(lambda-var ,(? 'args) ,(? 'exprs))
					(lambda(args exprs) `(lambda-var ,args ,(eliminate-nested-defines exprs))))
					
					(pattern-rule 
					`(applic ,(? 'func) ,(? 'exprs list?))
					(lambda(func exprs) `(applic ,(eliminate-nested-defines func) ,(map eliminate-nested-defines exprs))))
					
					(pattern-rule 
					`(or ,(? 'args list?))
					(lambda(args) `(or ,(map eliminate-nested-defines args))))
					
					(pattern-rule 
					`(set ,(? 'var) ,(? 'val))
					(lambda(var val) `(set ,(eliminate-nested-defines var) ,(eliminate-nested-defines val))))
				
				)))
				(lambda (e)
				(run e
						(lambda ()
							(error 'parse
									(format "I can't recognize this: ~s" e)))))))
						











						
								
(define remove-applic-lambda-nil
	(let ((run
			(compose-patterns
				(pattern-rule 
				`(var ,(? 'var))
				(lambda(var) `(var ,var)))
				
				(pattern-rule 
				`(const ,(? 'const))
				(lambda(const) `(const ,const)))
				
				(pattern-rule 
				`(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
				(lambda(test dit dif) `(if3 ,(remove-applic-lambda-nil test) ,(remove-applic-lambda-nil dit) ,(remove-applic-lambda-nil dif))))
				
				(pattern-rule 
				`(def ,(? 'var-name) ,(? 'val))
				(lambda(var-name val) `(def ,var-name ,(remove-applic-lambda-nil val))))
				
				
				(pattern-rule 
				`(lambda-simple ,(? 'args list?) ,(? 'exprs))
				(lambda(args exprs) 
				(if (null? args)
				(remove-applic-lambda-nil exprs)
				`(lambda-simple ,args ,(remove-applic-lambda-nil exprs)))))
				
				(pattern-rule 
				`(lambda-opt ,(? 'args list?) ,(? 'rest) ,(? 'exprs))
				(lambda(args rest exprs) `(lambda-opt ,args ,rest ,(remove-applic-lambda-nil exprs))))
				
				(pattern-rule 
				`(lambda-var ,(? 'args) ,(? 'exprs))
				(lambda(args exprs) `(lambda-var ,args ,(remove-applic-lambda-nil exprs))))
				
				(pattern-rule 
				`(applic ,(? 'func) ,(? 'exprs list?))
				(lambda(func exprs) `(applic ,(remove-applic-lambda-nil func) ,(map remove-applic-lambda-nil exprs))))
				
				(pattern-rule 
				`(or ,(? 'args list?))
				(lambda(args) `(or ,(map remove-applic-lambda-nil args))))
				
				(pattern-rule 
				`(set ,(? 'var) ,(? 'val))
				(lambda(var val) `(set ,(remove-applic-lambda-nil var) ,(remove-applic-lambda-nil val))))
				
				(pattern-rule 
				`(seq ,(? 'exprs list?))
				(lambda(exprs) `(seq ,(map remove-applic-lambda-nil exprs))))
				
				
				
			)))
		(lambda (e)
			(run e
					(lambda ()
						(error 'parse
								(format "I can't recognize this: ~s" e)))))))		
										
								
								
								

			

(define annotate-tc-val
	(lambda (pexpr is-tc)
		(let ((run
			(compose-patterns
				(pattern-rule 
				`(var ,(? 'var))
				(lambda(var) `(var ,var)))
				
				(pattern-rule 
				`(const ,(? 'const))
				(lambda(const) `(const ,const)))
				
				(pattern-rule 
				`(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
				(lambda(test dit dif) `(if3 ,(annotate-tc-val test #f) ,(annotate-tc-val dit is-tc) ,(annotate-tc-val dif is-tc))))
				
				(pattern-rule 
				`(def ,(? 'var-name) ,(? 'val))
				(lambda(var-name val) `(def ,var-name ,(annotate-tc-val val is-tc))))
				
				
				(pattern-rule 
				`(lambda-simple ,(? 'args list?) ,(? 'exprs))
				(lambda(args exprs) `(lambda-simple ,args ,(annotate-tc-val exprs #t))))
				
				(pattern-rule 
				`(lambda-opt ,(? 'args list?) ,(? 'rest) ,(? 'exprs))
				(lambda(args rest exprs) `(lambda-opt ,args ,rest ,(annotate-tc-val exprs #t))))
				
				(pattern-rule 
				`(lambda-var ,(? 'args) ,(? 'exprs))
				(lambda(args exprs) `(lambda-var ,args ,(annotate-tc-val exprs #t))))
				
				(pattern-rule 
				`(applic ,(? 'func) ,(? 'exprs list?))
				(lambda(func exprs) 
				(if is-tc `(tc-applic ,(annotate-tc-val func #f) ,(map (lambda (expr) (annotate-tc-val expr #f)) exprs))
				`(applic ,(annotate-tc-val func #f) ,(map  (lambda (expr) (annotate-tc-val expr #f)) exprs)))))
				
				;//TODO 'or'
				(pattern-rule 
				`(or ,(? 'args list?))
				(lambda(args) `(or ,(map (lambda (arg) (annotate-tc-val arg is-tc)) args))))
				
				(pattern-rule 
				`(set ,(? 'var) ,(? 'val))
				(lambda(var val) `(set ,(annotate-tc-val var #f) ,(annotate-tc-val val is-tc))))
				
				;//TODO 'seq'
				(pattern-rule 
				`(seq ,(? 'exprs list?))
				(lambda(exprs) `(seq ,(map remove-applic-lambda-nil exprs))))
				
				
				
			)))
			(run pexpr
					(lambda ()
						(error 'parse
								(format "I can't recognize this: ~s" e)))))))


			
(define annotate-tc
	(lambda (pexpr)
			(annotate-tc-val pexpr #f)))									
								
								
								
								
								
								
(define pe->lex-pe-val
	(lambda(pexpr bounds params)
			(let ((run
				(compose-patterns
					(pattern-rule 
					`(var ,(? 'var))
					(lambda(var) 
						(let ((p-index (fold-right (lambda (param val) 
							(if (equal? (car param) var) (cadr param)
								val)) -1 params))
							  (b-index (fold-right (lambda (bound val) 
							(if (equal? (car bound) var) (cdr bound)
								val)) '() bounds)))
								
						(cond ((not (= p-index -1)) `(pvar ,var ,p-index))
							  ((not (null? b-index)) `(bvar ,var ,(car b-index) ,(cadr b-index)))
							   (else `(fvar ,var)))
						)))
					
					(pattern-rule 
					`(const ,(? 'const))
					(lambda(const) `(const ,const)))
					
					(pattern-rule 
					`(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda(test dit dif) `(if3 ,(pe->lex-pe-val test bounds params) ,(pe->lex-pe-val dit bounds params) ,(pe->lex-pe-val dif bounds params))))
					
					(pattern-rule 
					`(def ,(? 'var-name) ,(? 'val))
					(lambda(var-name val) `(def ,var-name ,(pe->lex-pe-val val bounds params))))
					
					
					(pattern-rule 
					`(lambda-simple ,(? 'args list?) ,(? 'exprs))
					(lambda(args exprs) 
						(letrec ((indexer (lambda (lst index) (if (null? lst) lst (cons (list (car lst) index) (indexer (cdr lst) (+ index 1))))))
								 (merger (lambda (bounds params) (if (null? params) bounds (merger (cons (list (car (car params)) 0 (cadr (car params))) bounds) (cdr params)))))
								 (dup-remover (lambda (bounds params) (if (null? bounds) bounds 
																			(if (fold-right (lambda (param bool) (or bool (equal? param (car (car bounds))))) #f params) (dup-remover (cdr bounds) params) (cons (list (caar bounds) (+ (car (cdr (car bounds))) 1) (car (cdr (cdr (car bounds))))) (dup-remover (cdr bounds) params))))))
								)
								(let* ((new-params (indexer args 0))
									   (new-bounds (merger (dup-remover bounds new-params) params)))
								`(lambda-simple ,args ,(pe->lex-pe-val exprs new-bounds new-params))))))
					
					(pattern-rule 
					`(lambda-opt ,(? 'args list?) ,(? 'rest) ,(? 'exprs))
					(lambda(args rest exprs) 
						(letrec ((indexer (lambda (lst index) (if (null? lst) lst (cons (list (car lst) index) (indexer (cdr lst) (+ index 1))))))
								 (merger (lambda (bounds params) (if (null? params) bounds (merger (cons (list (car (car params)) 0 (cadr (car params))) bounds) (cdr params)))))
								 (dup-remover (lambda (bounds params) (if (null? bounds) bounds 
																			(if (fold-right (lambda (param bool) (or bool (equal? param (car (car bounds))))) #f params) (dup-remover (cdr bounds) params) (cons (list (caar bounds) (+ (car (cdr (car bounds))) 1) (car (cdr (cdr (car bounds))))) (dup-remover (cdr bounds) params))))))
								)
								(let* ((new-params (indexer (append args (list rest)) 0))
									   (new-bounds (merger (dup-remover bounds new-params) params)))
								`(lambda-opt ,args ,rest ,(pe->lex-pe-val exprs new-bounds new-params))))))
					
					(pattern-rule 
					`(lambda-var ,(? 'args) ,(? 'exprs))
					(lambda(args exprs) 
						(letrec ((indexer (lambda (lst index) (if (null? lst) lst (cons (list (car lst) index) (indexer (cdr lst) (+ index 1))))))
								 (merger (lambda (bounds params) (if (null? params) bounds (merger (cons (list (car (car params)) 0 (cadr (car params))) bounds) (cdr params)))))
								 (dup-remover (lambda (bounds params) (if (null? bounds) bounds 
																			(if (fold-right (lambda (param bool) (or bool (equal? param (car (car bounds))))) #f params) (dup-remover (cdr bounds) params) (cons (list (caar bounds) (+ (car (cdr (car bounds))) 1) (car (cdr (cdr (car bounds))))) (dup-remover (cdr bounds) params))))))
								)
								(let* ((new-params (indexer (list args) 0))
									   (new-bounds (merger (dup-remover bounds new-params) params)))
								`(lambda-var ,args ,(pe->lex-pe-val exprs new-bounds new-params))))))
					
					(pattern-rule 
					`(applic ,(? 'func) ,(? 'exprs list?))
					(lambda(func exprs) `(applic ,(pe->lex-pe-val func bounds params) ,(map (lambda(expr) (pe->lex-pe-val expr bounds params)) exprs))))
					
					;//TODO 'or'
					(pattern-rule 
					`(or ,(? 'args list?))
					(lambda(args) `(or ,(map (lambda (arg) (pe->lex-pe-val arg bounds params)) args))))
					
					(pattern-rule 
					`(set ,(? 'var) ,(? 'val))
					(lambda(var val) `(set ,(pe->lex-pe-val var bounds params) ,(pe->lex-pe-val val bounds params))))
					
					;//TODO 'seq'
					(pattern-rule 
					`(seq ,(? 'exprs list?))
					(lambda(exprs) `(seq ,(map (lambda(expr) (pe->lex-pe-val expr bounds params)) exprs))))
					
					
					
				)))
				(run pexpr
						(lambda ()
							(error 'parse
									(format "I can't recognize this: ~s" e)))))))
			
		
		
		
		
		
		
(define pe->lex-pe
	(lambda (pexpr)
		(pe->lex-pe-val pexpr '() `())))
								
								

								
								