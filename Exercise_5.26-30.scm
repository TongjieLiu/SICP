; An exercise solution for the SICP, 2nd edition
;
; Copyright (C) 2021 Tongjie Liu <tongjieandliu@gmail.com>.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.

; Exercise 5.26:
;     a. 10
;     b. total-number-of-push-operations = 44 * n + 37
;
;
; Exercise 5.27:
;     maximum depth(y): y = 8, n =1
;                       y = 5 * n + 5, n > 1
;
;     number of pushes(z): z = 41 * n - 20
;
;
; Exercise 5.29:
;     a. maximum depth(y): y = 8 * n + 3
;     b. i) S(n) = S(n - 1) + S(n - 2) + k,
;        and k = 53
;        ii) S(n) = a * Fib(n + 1) + b,
;        and a = 76, b = -53




; Table of Contents
; SECTION 1: basic tools
; SECTION 2: the explicit-control evaluator
; SECTION 3: the register-machine simulator
;
;
; Interfaces
;     Evaluator:     1. (eceval)
;
;     Simulator:     1. (make-machine op-table controller-text)
;                    2. (start machine)
;                    3. (get-register-contents machine reg-name)
;                    4. (set-register-contents! machine reg-name new-contents)
;                    5. (machine-get-instruction-count machine)
;                    6. (machine-reset-instruction-count! machine)
;                    7. (machine-trace-on machine)
;                    8. (machine-trace-off machine)
;                    9. (machine-trace-register-on machine reg-name)
;                   10. (machine-trace-register-off machine reg-name)
;                   11. (set-breakpoint machine breakpoint-name line-num)
;                   12. (proceed-machine machine)
;                   13. (cancel-breakpoint machine breakpoint-name line-num)
;                   14. (cancel-all-breakpoints machine)
 



(define PREDEFINED-PROCEDURES
    (list (list "tests"
                '(begin (define (tagged-list? alist type-tag)
                            (and (pair? alist)
	                         (let ((head (car alist)))
	                             (and (symbol? head)
	                                  (eq? head type-tag)))))

                        (define yes '(yes 1))
			(define no '(no a b c d e))

                        (define (assoc key records)
                            (cond ((null? records) #f)
	                          ((equal? key (car (car records)))
	                              (car records))
	                          (else (assoc key (cdr records)))))

			(define test-table (list (list 'one 1)
						 (list 'two 2)
						 (list 'three 3)))

		        'done))
          (list "factorial-5.26"
		'(define (factorial-5.26 n)
		    (define (iter product counter)
		        (if (> counter n)
			    product
			    (iter (* product counter)
				  (+ counter 1))))

		    (iter 1 1)))
	  (list "factorial-5.27"
		'(define (factorial-5.27 n)
		    (if (= n 1)
			1
			(* (factorial-5.27 (- n 1)) n))))
	  (list "fib"
		'(define (fib n)
		    (if (< n 2)
			n
			(+ (fib (- n 1))
			   (fib (- n 2))))))))




; --- SECTION 1: basic tools ---
(define (accumulate proc initial items)
    (if (null? items)
	initial
	(proc (car items)
	      (accumulate proc initial (cdr items)))))


(define (map proc items)
    (accumulate (lambda (x last)
		    (cons (proc x) last))
		'()
		items))


(define (for-each action items)
    (if (null? items)
	'done
	(begin (action (car items))
	       (for-each action (cdr items)))))



(define (append a b)
    (if (null? a)
	b
	(cons (car a)
	      (append (cdr a) b))))


(define (max first-number . rest-numbers)
    (accumulate (lambda (x last-max)
		    (if (> x last-max)
			x
			last-max))
		first-number
		rest-numbers))


(define (length items)
    (accumulate (lambda (x rest-length)
		    (+ rest-length 1))
		0
		items))


(define (printn first-text . rest-texts)
    (display first-text)
    (for-each (lambda (text)
		  (display " ")
		  (display text))
	      rest-texts)
    (newline))


(define (assoc key records)
    (cond ((null? records) #f)
	  ((equal? key (caar records))
	      (car records))
	  (else (assoc key (cdr records)))))



(define (tagged-list? alist type-tag)
    (and (pair? alist)
	 (let ((head (car alist)))
             ; how to handle of "eq?" in comparing numbers is not specified
	     (and (symbol? head)
	          (eq? head type-tag)))))


(define (adjoin alist item)
    (append alist (list item)))




; --- SECTION 2: the explicit-control evaluator ---
; evaluator interface #1
(define (eceval)
    (let ((machine (make-eceval)))
        (eceval-error-set-ecc (machine-get-register machine 'ecc))
	machine))


(define (make-eceval)
    (make-machine (list (list 'simple-apply apply)
		        (list 'true? (lambda (bool) (not (eq? bool #f))))
		        (list 'false? (lambda (bool) (eq? bool #f)))
			(list 'adjoin adjoin)
			(list 'make-list list)
                        (list 'self-evaluating? self-evaluating?)
			(list 'variable? variable?)
			(list 'quoted? quoted?)
			(list 'quoted-text quoted-text)
			(list 'assignment? assignment?)
			(list 'assignment-variable assignment-variable)
			(list 'assignment-value-expression
			      assignment-value-expression)
			(list 'definition? definition?)
			(list 'definition-variable definition-variable)
			(list 'definition-value-expression
			      definition-value-expression)
			(list 'lambda? lambda?)
			(list 'lambda-formal-parameters
			      lambda-formal-parameters)
			(list 'lambda-body lambda-body)
			(list 'if? if?)
			(list 'if-predicate if-predicate)
			(list 'if-consequent-expression
			      if-consequent-expression)
			(list 'if-alternative-expression
			      if-alternative-expression)
			(list 'begin? begin?)
			(list 'begin-expressions begin-expressions)
			(list 'sequence-last-expression?
			      sequence-last-expression?)
			(list 'sequence-no-expression?
			      sequence-no-expression?)
			(list 'sequence-first-expression
			      sequence-first-expression)
			(list 'sequence-rest-expressions
			      sequence-rest-expressions)
			(list 'let? let?)
			(list 'let->application
			      let->application)
			(list 'application? application?)
			(list 'application-operator
			      application-operator)
			(list 'application-operands
			      application-operands)
			(list 'application-no-operand?
			      application-no-operand?)
			(list 'application-first-operand
			      application-first-operand)
			(list 'application-rest-operands
			      application-rest-operands)
			(list 'cond? cond?)
			(list 'cond->if cond->if)
			(list 'or? or?)
			(list 'or-predicates
			      or-predicates)
			(list 'or-no-predicate?
			      or-no-predicate?)
			(list 'or-first-predicate
			      or-first-predicate)
			(list 'or-rest-predicates
			      or-rest-predicates)
                        (list 'and? and?)
			(list 'and-predicates
			      and-predicates)
			(list 'and-no-predicate?
			      and-no-predicate?)
			(list 'and-first-predicate
			      and-first-predicate)
			(list 'and-rest-predicates
			      and-rest-predicates)
			(list 'load? load?)
			(list 'load-procedure-name
			      load-procedure-name)
			(list 'load-get-procedure-definition
			      load-get-procedure-definition)
			(list 'primitive-procedure?
			      primitive-procedure?)
			(list 'primitive-procedure-implementation
			      primitive-procedure-implementation)
			(list 'make-compound-procedure
			      (make-compound-procedure-generator))
			(list 'compound-procedure?
			      compound-procedure?)
			(list 'compound-procedure-formal-parameters
			      compound-procedure-formal-parameters)
			(list 'compound-procedure-body
			      compound-procedure-body)
			(list 'compound-procedure-environment
			      compound-procedure-environment)
			(list 'compound-procedure-print
			      compound-procedure-print)
			(list 'environment-get-enclosing-environment
			      environment-get-enclosing-environment)
			(list 'environment-extend
			      environment-extend)
			(list 'environment-lookup-binding
			      environment-lookup-binding)
			(list 'environment-modify-binding
			      environment-modify-binding)
			(list 'environment-add-binding
			      environment-add-binding)
			(list 'environment-get-global-environment
			      (make-get-global-environment))
			(list 'eceval-user-print
			      eceval-user-print))
		  eceval-controller-sequence))



(define (make-eceval-error)
    (let ((ecc #f))
        (define (set-ecc ecc-object)
	    (if ecc
		(error "ECEVAL ERROR: ecc had been already setted")
		(set! ecc ecc-object)))

	(define (report error-messages)
	    (register-set-contents! ecc error-messages))

        (lambda (m)
            (cond ((eq? m 'set-ecc) set-ecc)
		  ((eq? m 'report) report)
		  (else (error "ECEVAL ERROR: unexpected message:"
			       m))))))


(define default-eceval-error (make-eceval-error))


(define (eceval-error-set-ecc ecc-object)
    ((default-eceval-error 'set-ecc) ecc-object))


(define (eceval-error . error-messages)
    ((default-eceval-error 'report) error-messages))



(define (eceval-user-print obj)
    (if (compound-procedure? obj)
	(compound-procedure-print obj)
	(printn obj))
    (newline))


(define eceval-controller-sequence
'(driver-loop
    (perform (op stack-initialize))

    (perform (op printn) (const "EC-EVAL INPUT>"))
    
    (assign exp (op read))
    (assign env (op environment-get-global-environment))
    (assign continue (label driver-loop-print-result))
    (goto (label eval))


driver-loop-print-result
    ;(perform (op stack-print-statistics))
    (perform (op print) (const "EC-EVAL RESULT> "))
    (perform (op eceval-user-print) (reg val))

    (goto (label driver-loop))




; modified for exercise 5.30, now error message is "list" not "string"
error-report
    (perform (op print) (const "ERROR>> "))


error-report-loop
    (test (op null?) (reg val))
    (branch (label error-report-done))

    (assign exp (op car) (reg val))
    (perform (op eceval-user-print) (reg exp))

    (assign val (op cdr) (reg val))
    (goto (label error-report-loop))


error-report-done
    (goto (label driver-loop))



; modified for exercise 5.30, now register "val" points to a list
error-unexpected-expression-type
    (assign val (const "ERROR: unexpected type of expression:"))
    (assign val (op make-list) (reg val) (reg exp))
    (goto (label error-report))


; modified for exercise 5.30, now register "val" points to a list
error-unexpected-procedure-type
    (assign val (const "ERROR: unexpected type of procedure:"))
    (assign val (op make-list) (reg val) (reg proc))
    (goto (label error-report))


; add for exercise 5.30
error-unbound-variable
    (assign val (reg ecc))
    (goto (label error-report))


; add for exercise 5.30
error-variables-values-do-not-match
    (assign val (reg ecc))
    (goto (label error-report))


; add for exercise 5.30
error-cond->if
    (assign val (reg ecc))
    (goto (label error-report))


; add for exercise 5.30
error-apply-primitive
    (assign val (reg ecc))
    (goto (label error-report))




eval
    (test (op self-evaluating?) (reg exp))
    (branch (label eval-self-evaluating))

    (test (op variable?) (reg exp))
    (branch (label eval-variable))

    (test (op quoted?) (reg exp))
    (branch (label eval-quoted))

    (test (op if?) (reg exp))
    (branch (label eval-if))

    (test (op cond?) (reg exp))
    (branch (label eval-cond))

    (test (op or?) (reg exp))
    (branch (label eval-or))

    (test (op and?) (reg exp))
    (branch (label eval-and))

    (test (op assignment?) (reg exp))
    (branch (label eval-assignment))

    (test (op definition?) (reg exp))
    (branch (label eval-definition))

    (test (op lambda?) (reg exp))
    (branch (label eval-lambda))

    (test (op begin?) (reg exp))
    (branch (label eval-begin))

    (test (op let?) (reg exp))
    (branch (label eval-let))

    ;     Currently supports loading predefined procedures in the
    ; table "PREDEFINED-PROCEDURES" to simplify the testing process.
    ;     SYNTAX: load <procedure-name>
    (test (op load?) (reg exp))
    (branch (label eval-load))

    (test (op application?) (reg exp))
    (branch (label eval-application))

    (goto (label error-unexpected-expression-type))




eval-self-evaluating
    (assign val (reg exp))
    (goto (reg continue))




eval-variable
    ; add error handlling for exercise 5.30
    (save env)
    (assign env (op environment-get-global-environment))
    (assign ecc (op environment-lookup-binding)
	            (reg env) (const false))
    (restore env)

    (assign val (op environment-lookup-binding)
	            (reg env) (reg exp))

    (test (op true?) (reg ecc))
    (branch (label error-unbound-variable))

    (goto (reg continue))




eval-quoted
    (assign val (op quoted-text) (reg exp))
    (goto (reg continue))




eval-lambda
    (assign unev (op lambda-formal-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-compound-procedure)
	            (reg unev) (reg exp) (reg env))
    (goto (reg continue))




eval-cond
    ; add error handling for exercise 5.30
    (save env)
    (assign env (op environment-get-global-environment))
    (assign ecc (op environment-lookup-binding)
	            (reg env) (const false))
    (restore env)
    (assign exp (op cond->if) (reg exp))
    (test (op true?) (reg ecc))
    (branch (label error-cond->if))

    (goto (label eval-if))




eval-let
    (assign exp (op let->application) (reg exp))
    (goto (label eval-application))




eval-or
    (save continue)
    (assign unev (op or-predicates) (reg exp))


eval-or-loop
    (test (op or-no-predicate?) (reg unev))
    (branch (label eval-or-all-false))

    (save env)
    (save unev)

    (assign exp (op or-first-predicate) (reg unev))
    (assign continue (label eval-or-loop-after-eval))
    (goto (label eval))


eval-or-loop-after-eval
    (restore unev)
    (restore env)

    (test (op true?) (reg val))
    (branch (label eval-or-found-true))

    (assign unev (op or-rest-predicates) (reg unev))
    (goto (label eval-or-loop))


eval-or-all-false
    (assign env (op environment-get-global-environment))
    (assign val (op environment-lookup-binding)
	            (reg env) (const false))

    (restore continue)
    (goto (reg continue))


eval-or-found-true
    (restore continue)
    (goto (reg continue))




eval-and
    (save continue)
    (assign unev (op and-predicates) (reg exp))


eval-and-loop
    (test (op and-no-predicate?) (reg unev))
    (branch (label eval-and-all-true))

    (save env)
    (save unev)

    (assign exp (op and-first-predicate) (reg unev))
    (assign continue (label eval-and-loop-after-eval))
    (goto (label eval))


eval-and-loop-after-eval
    (restore unev)
    (restore env)

    (test (op false?) (reg val))
    (branch (label eval-and-found-false))

    (assign unev (op and-rest-predicates) (reg unev))
    (goto (label eval-and-loop))


eval-and-all-true
    (assign env (op environment-get-global-environment))
    (assign val (op environment-lookup-binding)
	            (reg env) (const true))

    (restore continue)
    (goto (reg continue))


eval-and-found-false
    (restore continue)
    (goto (reg continue))




eval-application
    (save continue)
    (save env)
    (save exp)

    (assign exp (op application-operator) (reg exp))
    (assign continue (label eval-application-after-eval-operator))
    (goto (label eval))


eval-application-after-eval-operator
    (restore exp)
    (restore env)

    (assign proc (reg val))
    (save proc)

    (assign unev (op application-operands) (reg exp))
    (assign argl (const ()))


eval-application-operands-loop
    (test (op application-no-operand?) (reg unev))
    (branch (label apply))

    (save argl)
    (save env)
    (save unev)

    (assign exp (op application-first-operand) (reg unev))
    (assign continue (label eval-application-operands-loop-after-eval))
    (goto (label eval))


eval-application-operands-loop-after-eval
    (restore unev)
    (restore env)
    (restore argl)

    (assign argl (op adjoin) (reg argl) (reg val))
    (assign unev (op application-rest-operands) (reg unev))

    (goto (label eval-application-operands-loop))




apply
    (restore proc)

    (test (op primitive-procedure?) (reg proc))
    (branch (label apply-primitive-procedure))

    (test (op compound-procedure?) (reg proc))
    (branch (label apply-compound-procedure))

    (goto (label error-unexpected-procedure-type))


apply-primitive-procedure
    (assign proc (op primitive-procedure-implementation)
	             (reg proc))
    ; add error handling for exercise 5.30
    (save env)
    (assign env (op environment-get-global-environment))
    (assign ecc (op environment-lookup-binding)
	            (reg env) (const false))
    (restore env)
    (assign val (op simple-apply) (reg proc) (reg argl))
    (test (op true?) (reg ecc))
    (branch (label error-apply-primitive))

    (restore continue)
    (goto (reg continue))


apply-compound-procedure
    (assign exp (op compound-procedure-formal-parameters)
	            (reg proc))
    (assign env (op compound-procedure-environment)
	            (reg proc))
    ; add error handling for exercise 5.30
    (save env)
    (assign env (op environment-get-global-environment))
    (assign ecc (op environment-lookup-binding)
	            (reg env) (const false))
    (restore env)
    (assign env (op environment-extend)
	            (reg env) (reg exp) (reg argl))
    (test (op true?) (reg ecc))
    (branch (label error-variables-values-do-not-match))

    (assign unev (op compound-procedure-body) (reg proc))
    (restore continue)
    (goto (label eval-sequence))




eval-begin
    (assign unev (op begin-expressions) (reg exp))
    (goto (label eval-sequence))




eval-sequence
    (save continue)

    ; add for exercise 5.28
    ;(goto (label eval-sequence-loop-WTR))


eval-sequence-loop ; with tail recursion
    (test (op sequence-last-expression?) (reg unev))
    (branch (label eval-sequence-last-expression))

    (save env)
    (save unev)

    (assign exp (op sequence-first-expression) (reg unev))
    (assign continue (label eval-sequence-loop-after-eval))
    (goto (label eval))


eval-sequence-loop-after-eval
    (restore unev)
    (restore env)

    (assign unev (op sequence-rest-expressions) (reg unev))
    (goto (label eval-sequence-loop))


eval-sequence-last-expression
    (assign exp (op sequence-first-expression) (reg unev))
    (restore continue)
    (goto (label eval))



; Add for exercise 5.28.
; Suffix "WTR": without tail recursion.
eval-sequence-loop-WTR
    (test (op sequence-no-expression?) (reg unev))
    (branch (label eval-sequence-done-WTR))

    (save env)
    (save unev)

    (assign exp (op sequence-first-expression) (reg unev))
    (assign continue (label eval-sequence-loop-after-eval-WTR))
    (goto (label eval))


eval-sequence-loop-after-eval-WTR
    (restore unev)
    (restore env)

    (assign unev (op sequence-rest-expressions) (reg unev))
    (goto (label eval-sequence-loop-WTR))


eval-sequence-done-WTR
    (restore continue)
    (goto (reg continue))




eval-if
    (save continue)
    (save env)
    (save exp)

    (assign exp (op if-predicate) (reg exp))
    (assign continue (label eval-if-after-eval-predicate))
    (goto (label eval))


eval-if-after-eval-predicate
    (restore exp)
    (restore env)

    (test (op true?) (reg val))
    (branch (label eval-if-apply-consequent-expression))

    (assign exp (op if-alternative-expression) (reg exp))
    (restore continue)
    (goto (label eval))


eval-if-apply-consequent-expression
    (assign exp (op if-consequent-expression) (reg exp))
    (restore continue)
    (goto (label eval))




eval-assignment
    (save continue)
    (save env)
    (save exp)

    (assign exp (op assignment-value-expression) (reg exp))
    (assign continue (label eval-assignment-after-eval-value-expression))
    (goto (label eval))


eval-assignment-after-eval-value-expression
    (restore exp)
    (restore env)

    (assign exp (op assignment-variable) (reg exp))
    ; add error handling for exercise 5.30
    (save env)
    (assign env (op environment-get-global-environment))
    (assign ecc (op environment-lookup-binding)
	            (reg env) (const false))
    (restore env)
    (perform (op environment-modify-binding)
	         (reg env) (reg exp) (reg val))
    (test (op true?) (reg ecc))
    (branch (label error-unbound-variable))

    (restore continue)
    (goto (reg continue))




eval-definition
    (save continue)
    (save env)
    (save exp)

    (assign exp (op definition-value-expression) (reg exp))
    (assign continue (label eval-definition-after-eval-value-expression))
    (goto (label eval))


eval-definition-after-eval-value-expression
    (restore exp)
    (restore env)

    (assign exp (op definition-variable) (reg exp))
    (perform (op environment-add-binding)
	         (reg env) (reg exp) (reg val))

    (restore continue)
    (goto (reg continue))




eval-load
    (assign exp (op load-procedure-name) (reg exp))
    (assign exp (op load-get-procedure-definition) (reg exp))
    (goto (label eval))))




; syntax procedures
(define (self-evaluating? exp) (or (number? exp) (string? exp)))



(define (variable? exp) (symbol? exp))



(define (quoted? exp) (tagged-list? exp 'quote))


(define (quoted-text exp) (cadr exp))



(define (assignment? exp) (tagged-list? exp 'set!))


(define (assignment-variable exp) (cadr exp))


(define (assignment-value-expression exp) (caddr exp))



(define (definition? exp) (tagged-list? exp 'define))


(define (definition-variable exp)
    (if (variable? (cadr exp))
	(cadr exp)
	(caadr exp)))


(define (definition-value-expression exp)
    (if (variable? (cadr exp))
	(caddr exp)
	(make-lambda (cdadr exp) (cddr exp))))



(define (lambda? exp) (tagged-list? exp 'lambda))


(define (lambda-formal-parameters exp) (cadr exp))


(define (lambda-body exp) (cddr exp))


(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))



(define (if? exp) (tagged-list? exp 'if))


(define (if-predicate exp) (cadr exp))


(define (if-consequent-expression exp) (caddr exp))


(define (if-alternative-expression exp)
    (if (null? (cdddr exp))
	'none
	(cadddr exp)))


(define (make-if predicate consequence alternative)
    (list 'if predicate consequence alternative))



(define (or? exp) (tagged-list? exp 'or))


(define (or-predicates exp) (cdr exp))


(define (or-no-predicate? predicates) (null? predicates))


(define (or-first-predicate predicates) (car predicates))


(define (or-rest-predicates predicates) (cdr predicates))



(define (and? exp) (tagged-list? exp 'and))


(define (and-predicates exp) (cdr exp))


(define (and-no-predicate? predicates) (null? predicates))


(define (and-first-predicate predicates) (car predicates))


(define (and-rest-predicates predicates) (cdr predicates))




(define (begin? exp) (tagged-list? exp 'begin))


(define (begin-expressions exp) (cdr exp))


(define (make-begin seq) (cons 'begin seq))


; used in exercise 5.28 for removing tail-recursion
(define (sequence-no-expression? seq) (null? seq))


;     It is needed in implementing tail recursion, the last
; expression are handled specially for this purpose
(define (sequence-last-expression? seq)
    (if (null? seq)
	(error "SEQUENCE: there is no expression")
	(null? (cdr seq))))


(define (sequence-first-expression seq) (car seq))


(define (sequence-rest-expressions seq) (cdr seq))


(define (sequence->exp seq)
    (cond ((null? seq) #t)
	  ((sequence-last-expression? seq)
	      (sequence-first-expression seq))
	  (else (make-begin seq))))



(define (let? exp) (tagged-list? exp 'let))


(define (let-bindings exp) (cadr exp))


(define (let-parameters exp)
    (map car (let-bindings exp)))


(define (let-arguments exp)
    (map cadr (let-bindings exp)))


(define (let-body exp) (cddr exp))


(define (let->application exp)
    (make-application (make-lambda (let-parameters exp)
				   (let-body exp))
		      (let-arguments exp)))



(define (application? exp) (pair? exp))


(define (application-operator exp) (car exp))


(define (application-operands exp) (cdr exp))


(define (application-no-operand? operands) (null? operands))


(define (application-first-operand operands) (car operands))


(define (application-rest-operands operands) (cdr operands))


(define (make-application operator operands)
    (cons operator operands))



(define (cond? exp) (tagged-list? exp 'cond))


(define (cond-clauses exp) (cdr exp))


(define (cond-else-clause? clause) (tagged-list? clause 'else))


(define (cond-no-clause? clauses) (null? clauses))


(define (cond-first-clause clauses) (car clauses))


(define (cond-rest-clauses clauses) (cdr clauses))


(define (cond-clause-predicate clause) (car clause))


(define (cond-clause-expressions clause) (cdr clause))


(define (cond->if exp)
    (define (iter clauses)
        (if (cond-no-clause? clauses)
	    'false
	    (let ((first (cond-first-clause clauses))
		  (rest (cond-rest-clauses clauses)))
	        (if (cond-else-clause? first)
		    (if (cond-no-clause? rest)
			(sequence->exp (cond-clause-expressions first))
			(eceval-error
			    "COND: the else clause is not at the last"))
		    (make-if (cond-clause-predicate first)
			     (sequence->exp (cond-clause-expressions first))
			     (iter rest))))))

    (iter (cond-clauses exp)))



(define (load? exp) (tagged-list? exp 'load))


(define (load-procedure-name exp) (cadr exp))


(define (load-get-procedure-definition name)
    (let ((record (assoc name PREDEFINED-PROCEDURES)))
        (if record
	    (cadr record)
	    (eceval-error "LOAD: unexpected predefined procedure name:"
		          name))))




; primitive procedure representation
(define (make-primitive-procedure implementation)
    (list 'primitive-procedure implementation))


(define (primitive-procedure? obj)
    (tagged-list? obj 'primitive-procedure))


(define (primitive-procedure-implementation pproc) (cadr pproc))




; compound procedure representation
(define (make-compound-procedure-generator)
    (let ((cproc-number 0))
        (lambda (parameters body env)
	    (let ((n cproc-number))
	        (set! cproc-number (+ cproc-number 1))
	        (list 'compound-procedure parameters body env n)))))


(define (compound-procedure? obj)
    (tagged-list? obj 'compound-procedure))


(define (compound-procedure-formal-parameters cproc) (cadr cproc))


(define (compound-procedure-body cproc) (caddr cproc))


(define (compound-procedure-environment cproc) (cadddr cproc))


(define (compound-procedure-number cproc) (car (cddddr cproc)))


(define (compound-procedure-print cproc)
    (display "[COMPOUND PROCEDURE #")
    (display (compound-procedure-number cproc))
    (display "]\n"))




; frame representation
(define (make-frame vars vals) (cons vars vals))


(define (frame-variables frame) (car frame))


(define (frame-values frame) (cdr frame))


(define (frame-add-binding frame var val)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))




; environment representation
(define the-empty-environment '())


(define (environment-null? env) (null? env))


(define (environment-get-enclosing-environment env) (cdr env))


(define (environment-get-first-frame env) (car env))


(define (environment-add-frame env frame) (cons frame env))


(define (environment-extend env vars vals)
    (if (= (length vars) (length vals))
	(environment-add-frame env (make-frame vars vals))
	(eceval-error
	    "ENVIRONMENT EXTEND: variables and values do not match")))


(define (environment-lookup-binding env var)
    (define (find-frame env)
        (define (find-binding vars vals)
	    (cond ((null? vars)
		      (find-frame (environment-get-enclosing-environment env)))
		  ((eq? var (car vars))
                      (car vals))
		  (else (find-binding (cdr vars) (cdr vals)))))

	(if (environment-null? env)
	    (eceval-error "ENVIRONMENT LOOKUP BINDING: unbound variable:" var)
	    (let ((frame (environment-get-first-frame env)))
	        (find-binding (frame-variables frame)
			      (frame-values frame)))))

    (find-frame env))


(define (environment-modify-binding env var new-val)
    (define (find-frame env)
        (define (find-binding vars vals)
	    (cond ((null? vars)
		      (find-frame (environment-get-enclosing-environment env)))
		  ((eq? var (car vars))
                      (set-car! vals new-val))
		  (else (find-binding (cdr vars) (cdr vals)))))

	(if (environment-null? env)
	    (eceval-error "ENVIRONMENT MODIFY BINDING: unbound variable:" var)
	    (let ((frame (environment-get-first-frame env)))
	        (find-binding (frame-variables frame)
			      (frame-values frame)))))

    (find-frame env))


(define (environment-add-binding env var val)
    (let ((frame (environment-get-first-frame env)))
        (define (find-binding vars vals)
            (cond ((null? vars)
	              (frame-add-binding frame var val))
	          ((eq? var (car vars))
	              (set-car! vals val))
	          (else (find-binding (cdr vars) (cdr vals)))))

	(find-binding (frame-variables frame)
		      (frame-values frame))))


(define (make-global-environment)
    (let ((eceval-builtin-variables
	     (list (list 'true #t)
		   (list 'false #f)))
	  (eceval-primitive-procedures
	     (list (list '< eceval-<)
		   (list '> eceval->)
		   (list '= eceval-=)
		   (list '<= eceval-<=)
		   (list '>= eceval->=)
		   (list 'not not)
		   (list '+ eceval-+)
		   (list '- eceval--)
		   (list '* eceval-*)
		   (list '/ eceval-/)
		   (list 'remainder eceval-remainder)
	           (list 'car eceval-car)
		   (list 'cdr eceval-cdr)
		   (list 'set-car! eceval-set-car!)
		   (list 'set-cdr! eceval-set-cdr!)
		   (list 'cons cons)
		   (list 'eq? eq?)
		   (list 'equal? equal?)
		   (list 'pair? pair?)
		   (list 'null? null?)
		   (list 'number? number?)
		   (list 'symbol? symbol?)
		   (list 'list list)
		   (list 'read read)
		   (list 'print display)
		   (list 'printn printn))))
        (let ((var-names (map car
			      eceval-builtin-variables))
	      (var-objects (map cadr
				eceval-builtin-variables))
	      (pproc-names (map car
			       eceval-primitive-procedures))
	      (pproc-objects (map (lambda (binding)
				      (make-primitive-procedure
					  (cadr binding)))
				  eceval-primitive-procedures)))
	    (environment-extend the-empty-environment
				(append var-names pproc-names)
				(append var-objects pproc-objects)))))


(define (make-get-global-environment)
    (let ((env (make-global-environment)))
        (lambda () env)))




(define (eceval-< a b)
    (if (and (number? a) (number? b))
	(< a b)
	(eceval-error "<: arguments need both be number")))


(define (eceval-> a b)
    (if (and (number? a) (number? b))
	(> a b)
	(eceval-error ">: arguments need both be number")))

(define (eceval-= a b)
     (if (and (number? a) (number? b))
	(= a b)
	(eceval-error "=: arguments need both be number")))

(define (eceval-<= a b)
    (if (and (number? a) (number? b))
	(<= a b)
	(eceval-error "<=: arguments need both be number")))

(define (eceval->= a b)
    (if (and (number? a) (number? b))
	(>= a b)
	(eceval-error ">=: arguments need both be number")))

(define (eceval-+ a b)
    (if (and (number? a) (number? b))
	(+ a b)
	(eceval-error "+: arguments need both be number")))

(define (eceval-- a b)
     (if (and (number? a) (number? b))
	(- a b)
	(eceval-error "-: arguments need both be number")))

(define (eceval-* a b)
    (if (and (number? a) (number? b))
	(* a b)
	(eceval-error "*: arguments need both be number")))

(define (eceval-/ a b)
    (if (and (number? a) (number? b))
	(if (= b 0)
	    (eceval-error "/: divided by zero")
	    (/ a b))
	(eceval-error "/: arguments need both be number")))

(define (eceval-remainder a b)
    (if (and (number? a) (number? b))
	(remainder a b)
	(eceval-error "remainder: arguments need both be number")))

(define (eceval-car x)
    (if (pair? x)
	(car x)
	(eceval-error "car: the argument need be a pair")))

(define (eceval-cdr x)
    (if (pair? x)
	(cdr x)
	(eceval-error "cdr: the argument need be a pair")))

(define (eceval-set-car! p val)
    (if (pair? p)
	(set-car! p val)
	(eceval-error "set-car!: the first argument need be a pair")))

(define (eceval-set-cdr! p val)
    (if (pair? p)
	(set-cdr! p val)
	(eceval-error "set-cdr!: the first argument need be a pair")))




; --- SECTION 3: the register-machine simulator ---
; simulator interface #1
(define (make-machine op-table controller-text)
    (let ((machine (make-basic-machine)))
        (machine-install-operations machine op-table)
	(machine-install-instruction-sequence machine
	                                      (assemble controller-text machine))
	machine))




(define (make-register reg-name)
    (let ((register-contents '*UNASSIGNED*)
	  (register-trace-enabled #f))
        (define (dispatch m)
            (cond ((eq? m 'get-contents)
		      (if (eq? register-contents '*UNASSIGNED*)
			  (error "REGISTER: unassigned register:"
                                 reg-name)
			  register-contents))
		  ((eq? m 'set-contents!)
		      (lambda (new-contents)
			  (if register-trace-enabled
			      (begin (printn "[DEBUG INFO]" "REGISTER"
					     "name:" reg-name
					     "old:" register-contents
				             "new:" new-contents)))
			  (set! register-contents new-contents)))
		  ((eq? m 'trace)
		      (lambda (m)
			  (cond ((eq? m 'on)
				    (set! register-trace-enabled #t)
				    "REGISTER: trace on")
				((eq? m 'off)
				    (set! register-trace-enabled #f)
				    "REGISTER: trace off")
				(else (error "REGISTER TRACE: unexpected message:" m)))))
		  (else (error "REGISTER: unexpected message:"
			       m))))

	dispatch))


(define (register-get-contents reg)
    (reg 'get-contents))


(define (register-set-contents! reg new-contents)
    ((reg 'set-contents!) new-contents))


(define (register-trace-on reg)
    ((reg 'trace) 'on))


(define (register-trace-off reg)
    ((reg 'trace) 'off))




(define (make-stack)
    (let ((stack '())
	  (save-number 0)
	  (current-depth 0)
	  (max-depth 0))
        (define (push value)
	    (set! stack (cons value stack))
	    (set! save-number (+ save-number 1))
	    (set! current-depth (+ current-depth 1))
	    (set! max-depth (max current-depth max-depth)))

	(define (pop)
	    (if (null? stack)
		(error "STACK: empty stack")
		(let ((value (car stack)))
		    (set! stack (cdr stack))
		    (set! current-depth (- current-depth 1))
		    value)))

	(define (initialize)
	    (set! stack '())
	    (set! save-number 0)
	    (set! current-depth 0)
	    (set! max-depth 0)
	    'done)

	(define (print-statistics)
	   (printn "[DEBUG INFO]" "STACK"
                   "max-depth:" max-depth
                   "save-number:" save-number))

	(define (dispatch m)
	    (cond ((eq? m 'push) push)
		  ((eq? m 'pop) (pop))
		  ((eq? m 'initialize) (initialize))
		  ((eq? m 'print-statistics) (print-statistics))
		  (else (error "STACK: unexpected message:" m))))


	dispatch))


(define (stack-push stack value)
    ((stack 'push) value))


(define (stack-pop stack)
    (stack 'pop))


(define (stack-initialize stack)
    (stack 'initialize))


(define (stack-print-statistics stack)
   (stack 'print-statistics))




(define (make-basic-machine)
    (let ((machine-stack (make-stack))
	  (machine-instruction-sequence '())
	  (machine-labels '())
	  (machine-instruction-count 0)
	  (machine-trace-enabled #f)
	  (machine-suspended-next-procedure #f)
	  (pc (make-register 'pc))
	  (flag (make-register 'flag))
          ; Add for exercise 5.30.
          ; Register "ecc": error condition code.
	  (ecc (make-register 'ecc)))
        (let ((machine-operation-table
		 (list (list '< <)
		       (list '<= <=)
		       (list '> >)
		       (list '>= >=)
		       (list '= =)
		       (list '+ +)
		       (list '- -)
		       (list '* *)
		       (list '/ /)
		       (list 'rem remainder)
		       ;     simplify explicit-control evaluater by directly
		       ; using list operatiions provided by underlying scheme
		       ; since calling subroutines implemented in the book is
		       ; more complicated than simply using operation subex-
		       ; pression from register-machine language and which
		       ; choice we made is irrelevent to these exercises.
		       (list 'car car)
		       (list 'cdr cdr)
		       (list 'set-car! set-car!)
		       (list 'set-cdr! set-cdr!)
		       (list 'cons cons)
		       (list 'eq? eq?)
		       (list 'pair? pair?)
		       (list 'null? null?)
		       (list 'number? number?)
		       (list 'symbol? symbol?)
		       (list 'read read)
		       (list 'print display)
		       (list 'printn printn)
		       (list 'stack-initialize
		             (lambda ()
			         (stack-initialize machine-stack)))
		       (list 'stack-print-statistics
			     (lambda ()
			         (stack-print-statistics machine-stack)))))
	       (machine-register-table
		   (list (list 'pc pc)
		         (list 'flag flag)
			 (list 'ecc ecc))))


	    (define (allocate-register reg-name)
	        (let ((record (assoc reg-name machine-register-table)))
		    (if record
			(cadr record)
			(if (symbol? reg-name)
			    (let ((new-reg (make-register reg-name)))
			        (set! machine-register-table
			              (cons (list reg-name new-reg)
					    machine-register-table))
			        new-reg)
			    (error "MACHINE: illegal register name:"
				   reg-name)))))


	    (define (get-register reg-name)
	        (let ((record (assoc reg-name machine-register-table)))
		    (if record
			(cadr record)
			(error "MACHINE: No such register named:"
			       reg-name))))


	    (define (execute)
	        (let ((insts (register-get-contents pc)))
		    (if (null? insts)
			'success
			(let ((next-inst (car insts)))
			    (let ((next-proc
				     (instruction-execution-procedure next-inst)))
			        (set! machine-instruction-count
			              (+ machine-instruction-count 1))
			        (if machine-trace-enabled
			            (begin (let ((label-name (instruction-label-name next-inst)))
					       (if label-name
					           (printn label-name)))
				           (printn "\t" (instruction-text next-inst))))
				(let ((breakpoint-name (instruction-breakpoint-name next-inst)))
				    (if breakpoint-name
					(begin (printn "BREAKPOINT NAME:" breakpoint-name)
					       (set! machine-suspended-next-procedure
						     next-proc))
			                (begin (next-proc)
			                       (execute)))))))))


	    (define (dispatch m)
	        (cond ((eq? m 'get-stack) machine-stack)
		      ((eq? m 'get-operations) machine-operation-table)
		      ((eq? m 'get-labels) machine-labels)
		      ((eq? m 'get-instruction-sequence) machine-instruction-sequence)
		      ((eq? m 'get-suspended-next-procedure)
		          machine-suspended-next-procedure)
		      ((eq? m 'reset-suspended-next-procedure!)
		          (set! machine-suspended-next-procedure #f))
		      ((eq? m 'allocate-register) allocate-register)
		      ((eq? m 'get-register) get-register)
		      ((eq? m 'start)
		          (set! machine-suspended-next-procedure #f)
		          (register-set-contents! pc machine-instruction-sequence)
			  (execute))
		      ((eq? m 'install-labels)
		          (lambda (labels)
			      (set! machine-labels
				    labels)))
		      ((eq? m 'install-operations)
		          (lambda (ops)
			      (set! machine-operation-table
				    (append machine-operation-table
					    ops))))
		      ((eq? m 'install-instruction-sequence)
		          (lambda (seq)
			      (set! machine-instruction-sequence
				    seq)))
		      ((eq? m 'get-instruction-count)
		          (lambda (m)
			      (cond ((eq? m '=) machine-instruction-count)
				    ((eq? m 'reset)
				        (set! machine-instruction-count 0)
				        'reseted)
				    (else (error "GET INSTRUCTION COUNT: unexpected message:"
                                                 m)))))
		      ((eq? m 'trace)
		          (lambda (m)
			      (cond ((eq? m 'on)
				        (set! machine-trace-enabled #t)
					"machine: trace on")
				    ((eq? m 'off)
					(set! machine-trace-enabled #f)
					"machine: trace off")
				    (else (error "MACHINE TRACE: unexpected message")))))
		      ((eq? m 'execute)
		          (execute))
		      (else (error "MACHINE: unexpected message:" m))))


	    dispatch)))


(define (machine-get-stack machine)
    (machine 'get-stack))


(define (machine-get-operations machine)
    (machine 'get-operations))


(define (machine-get-labels machine)
    (machine 'get-labels))


(define (machine-get-instruction-sequence machine)
    (machine 'get-instruction-sequence))


(define (machine-get-suspended-next-procedure machine)
    (machine 'get-suspended-next-procedure))


(define (machine-reset-suspended-next-procedure! machine)
    (machine 'reset-suspended-next-procedure!))


(define (machine-allocate-register machine reg-name)
    ((machine 'allocate-register) reg-name))


(define (machine-get-register machine reg-name)
    ((machine 'get-register) reg-name))


(define (machine-install-labels machine labels)
    ((machine 'install-labels) labels))


(define (machine-install-operations machine ops)
    ((machine 'install-operations) ops))


(define (machine-install-instruction-sequence machine seq)
    ((machine 'install-instruction-sequence) seq))


(define (machine-execute machine)
    (machine 'execute))



; simulator interface #2
(define (start machine) (machine 'start))


; simulator interface #3
(define (get-register-contents machine reg-name)
    (register-get-contents (machine-get-register machine reg-name)))


; simulator interface #4
(define (set-register-contents! machine reg-name new-contents)
    (register-set-contents! (machine-get-register machine reg-name)
			   new-contents))


; simulator interface #5
(define (machine-get-instruction-count machine)
    ((machine 'get-instruction-count) '=))


; simulator interface #6
(define (machine-reset-instruction-count! machine)
    ((machine 'get-instruction-count) 'reset))


; simulator interface #7
(define (machine-trace-on machine)
    ((machine 'trace) 'on))


; simulator interface #8
(define (machine-trace-off machine)
    ((machine 'trace) 'off))


; simulator interface #9
(define (machine-trace-register-on machine reg-name)
    ;     Most registers are not allocated before the first time we "start"
    ; the machine. For this reason, first, we have to allocate the requested
    ; register, then we can turn its trace option "on" as we need.
    (let ((reg (machine-allocate-register machine reg-name)))
        ((reg 'trace) 'on)))


; simulator interface #10
(define (machine-trace-register-off machine reg-name)
    (let ((reg (machine-get-register machine reg-name)))
        ((reg 'trace) 'off)))


(define (find-instruction insts line-num)
    (if (= line-num 1)
	(car insts)
	(find-instruction (cdr insts) (- line-num 1))))

; simulator interface #11
(define (set-breakpoint machine breakpoint-name line-num)
    (let ((insts (machine-get-instruction-sequence machine)))
        (instruction-set-breakpoint-name! (find-instruction insts line-num)
					  breakpoint-name)
	'done))


; simulator interface #12
(define (proceed-machine machine)
    (let ((next-proc (machine-get-suspended-next-procedure machine)))
        (if next-proc
            (begin (next-proc)
	           (machine-reset-suspended-next-procedure! machine)
	           (machine-execute machine))
	    (error "MACHINE: machine has not met breakpoints yet"))))


; simulator interface #13
(define (cancel-breakpoint machine breakpoint-name line-num)
    (let ((insts (machine-get-instruction-sequence machine)))
        (instruction-set-breakpoint-name! (find-instruction insts line-num)
					  #f)
	'done))


; simulator interface #14
(define (cancel-all-breakpoints machine)
    (let ((insts (machine-get-instruction-sequence machine)))
        (for-each (lambda (inst)
		      (instruction-set-breakpoint-name! inst
							#f))
		  insts)
	'done))
 



(define (assemble controller-text machine)
    (separate-controller-text controller-text
			      (lambda (insts labels)
				  (machine-install-labels machine labels)
		                  (transform-instruction-sequence machine insts labels)
				  insts)))



(define (separate-controller-text text continuation)
    (if (null? text)
	(continuation '() '())
	(separate-controller-text
	    (cdr text)
	    (lambda (insts labels)
		(let ((inst (car text)))
		    (if (symbol? inst)
			(continuation insts
				      (cons (make-label inst insts)
                                            labels))
			(continuation (cons (make-instruction inst)
					    insts)
				      labels)))))))



(define (transform-instruction-sequence machine insts labels)
    (for-each (lambda (inst)
		  (instruction-set-execution-procedure!
		      inst
		      (generate-instruction-execution-procedure
			  machine (instruction-text inst) labels)))
	      insts))



; (list "text" "instruction-execution-procedure" "label-name" "breakpoint-name")
(define (make-instruction text)
    (list text #f #f #f))


(define (instruction-text inst)
    (car inst))


(define (instruction-execution-procedure inst)
    (cadr inst))


(define (instruction-set-execution-procedure! inst proc)
    (set-car! (cdr inst) proc))


(define (instruction-label-name inst)
    (caddr inst))


(define (instruction-set-label-name! inst label-name)
    (set-car! (cddr inst) label-name))


(define (instruction-breakpoint-name inst)
    (cadddr inst))


(define (instruction-set-breakpoint-name! inst breakpoint-name)
    (set-car! (cdddr inst) breakpoint-name))



(define (make-label label-name insts)
    (instruction-set-label-name! (car insts) label-name)
    (cons label-name insts))


(define (label-lookup labels label-name)
    (let ((record (assoc label-name labels)))
        (if record
	    (cdr record)
            (error "LABEL: no such label named:"
		   label-name))))




(define (generate-instruction-execution-procedure machine inst-text labels)
    (cond ((instruction-text-type-assign? inst-text)
	      (analyze-assign machine inst-text labels))
	  ((instruction-text-type-test? inst-text)
	      (analyze-test machine inst-text))
	  ((instruction-text-type-branch? inst-text)
	      (analyze-branch machine inst-text labels))
	  ((instruction-text-type-goto? inst-text)
	      (analyze-goto machine inst-text labels))
	  ((instruction-text-type-perform? inst-text)
	      (analyze-perform machine inst-text))
	  ((instruction-text-type-save? inst-text)
	      (analyze-save machine inst-text))
	  ((instruction-text-type-restore? inst-text)
	      (analyze-restore machine inst-text))
	  (else (error "ASSEMBLE: unexpected instruction type:"
		       inst-text))))


(define (instruction-text-type? inst type)
    (and (pair? inst)
	 (let ((first-item (car inst)))
	     (and (symbol? first-item)
	          (eq? first-item type)))))


(define (instruction-text-type-assign? inst)
    (instruction-text-type? inst 'assign))


(define (instruction-text-type-test? inst)
    (instruction-text-type? inst 'test))


(define (instruction-text-type-branch? inst)
    (instruction-text-type? inst 'branch))


(define (instruction-text-type-goto? inst)
    (instruction-text-type? inst 'goto))


(define (instruction-text-type-perform? inst)
    (instruction-text-type? inst 'perform))


(define (instruction-text-type-save? inst)
    (instruction-text-type? inst 'save))


(define (instruction-text-type-restore? inst)
    (instruction-text-type? inst 'restore))




; try to allocate "accept register", since there is no pre-allocation of machine registers
(define (analyze-assign machine inst-text labels)
    (let ((memo-reg #f)
	  (reg-name (assign-accept-register-name inst-text))
	  (value-exp (assign-value-expression inst-text))
	  (pc (machine-get-register machine 'pc)))
        (let ((value-proc
		 (cond ((expression-operation? value-exp)
		           (expression-operation-analyze machine value-exp))
		       ((pair? value-exp)
		           (let ((exp (car value-exp)))
		               (cond ((expression-register? exp)
			                 (expression-register-analyze machine exp))
			             ((expression-constant? exp)
				         (expression-constant-analyze exp))
			             ((expression-label? exp)
				         (expression-label-analyze exp labels))
				     (else (error "ASSEMBLE: illegal instruction error:"
						  inst-text)))))
		       (else (error "ASSEMBLE: illegal instruction format:"
				    inst-text)))))
	    (lambda ()
	        (if (not memo-reg)
	            (set! memo-reg (machine-allocate-register machine reg-name)))
	        (register-set-contents! memo-reg (value-proc))
                (advance-pc! pc)))))


(define (assign-accept-register-name inst-text)
    (cadr inst-text))


(define (assign-value-expression inst-text)
    (cddr inst-text))


(define (advance-pc! pc)
    (register-set-contents! pc
			    (cdr (register-get-contents pc))))




(define (analyze-test machine inst-text)
    (let ((value-exp (test-value-expression inst-text))
	  (flag (machine-get-register machine 'flag))
	  (pc (machine-get-register machine 'pc)))
        (let ((value-proc 
		 (if (expression-operation? value-exp)
		     (expression-operation-analyze machine value-exp)
		     (error "ASSEMBLE: illegal instruction format:"
			    inst-text))))
	    (lambda ()
	        (register-set-contents! flag (value-proc))
		(advance-pc! pc)))))


(define (test-value-expression inst-text)
    (cdr inst-text))




(define (analyze-branch machine inst-text labels)
    (let ((dest-exp (branch-destination-expression inst-text))
	  (flag (machine-get-register machine 'flag))
	  (pc (machine-get-register machine 'pc)))
        (let ((dest-proc
		 (if (expression-label? dest-exp)
	             (expression-label-analyze dest-exp labels)
	             (error "ASSEMBLE: illegal expression format:"
			    inst-text))))
            (lambda ()
	        (if (register-get-contents flag)
		    (register-set-contents! pc (dest-proc))
		    (advance-pc! pc))))))


(define (branch-destination-expression inst-text)
    (cadr inst-text))




(define (analyze-goto machine inst-text labels)
    (let ((dest-exp (goto-destination-expression inst-text))
	  (pc (machine-get-register machine 'pc)))
        (let ((dest-proc
		 (cond ((expression-register? dest-exp)
			   (expression-register-analyze machine dest-exp))
		       ((expression-label? dest-exp)
			   (expression-label-analyze dest-exp labels))
		       (else (error "ASSEMBLE: illegal expression format:"
			            inst-text)))))
	    (lambda ()
	        (register-set-contents! pc (dest-proc))))))


(define (goto-destination-expression inst-text)
    (cadr inst-text))




(define (analyze-perform machine inst-text)
    (let ((action-exp (perform-action-expression inst-text))
	  (pc (machine-get-register machine 'pc)))
        (let ((action-proc
	         (if (expression-operation? action-exp)
		     (expression-operation-analyze machine action-exp)
		     (error "ASSEMBLE: illegal instruction format:"
			    inst-text))))
	    (lambda ()
	        (action-proc)
		(advance-pc! pc)))))


(define (perform-action-expression inst-text)
    (cdr inst-text))




(define (analyze-save machine inst-text)
    (let ((memo-reg #f)
	  (reg-name (save-register-name inst-text))
	  (stack (machine-get-stack machine))
	  (pc (machine-get-register machine 'pc)))
        (lambda ()
	    (if (not memo-reg)
	        (set! memo-reg (machine-get-register machine reg-name)))
	    (stack-push stack (register-get-contents memo-reg))
            (advance-pc! pc))))


(define (save-register-name inst-text)
    (cadr inst-text))



; try to allocate register, since there is no pre-allocation of machine registers
(define (analyze-restore machine inst-text)
    (let ((memo-reg #f)
	  (reg-name (restore-register-name inst-text))
	  (stack (machine-get-stack machine))
	  (pc (machine-get-register machine 'pc)))
        (lambda ()
	    (if (not memo-reg)
	        (set! memo-reg (machine-allocate-register machine reg-name)))
	    (register-set-contents! memo-reg
				    (stack-pop stack))
	    (advance-pc! pc))))


(define (restore-register-name inst-text)
    (cadr inst-text))




(define (expression-primitive? exp type)
    (and (pair? exp)
	 (eq? (car exp) type)
	 (= (length exp) 2)))


(define (expression-register? exp)
    (expression-primitive? exp 'reg))


(define (expression-constant? exp)
    (expression-primitive? exp 'const))


(define (expression-label? exp)
    (expression-primitive? exp 'label))



(define (expression-primitive-operand exp)
    (cadr exp))


(define (expression-register-name exp)
    (expression-primitive-operand exp))


(define (expression-constant-value exp)
    (expression-primitive-operand exp))


(define (expression-label-name exp)
    (expression-primitive-operand exp))



(define (expression-register-analyze machine exp)
    (let ((memo-reg #f)
	  (reg-name (expression-register-name exp)))
        (lambda ()
	    (if (not memo-reg)
	        (set! memo-reg (machine-get-register machine reg-name)))
            (register-get-contents memo-reg))))


(define (expression-constant-analyze exp)
    (let ((value (expression-constant-value exp)))
        (lambda () value)))


(define (expression-label-analyze exp labels)
    (let ((insts (label-lookup labels
			       (expression-label-name exp))))
        (lambda () insts)))




(define (expression-operation? exp)
    (and (pair? exp)
	 (let ((op-name-exp (car exp)))
	     (and (pair? op-name-exp)
		  (eq? (car op-name-exp) 'op)
		  (= (length op-name-exp) 2)))))


(define (expression-operation-name exp)
    (cadr (car exp)))


(define (expression-operation-arguments exp)
    (cdr exp))


(define (expression-operation-analyze machine exp)
    (let ((proc (primitive-operation-lookup (machine-get-operations machine)
					    (expression-operation-name exp)))
	  (arg-procs (map (lambda (arg)
			      (cond ((expression-register? arg)
				        (expression-register-analyze machine arg))
				    ((expression-constant? arg)
				        (expression-constant-analyze arg))
				    (else (error "ASSEMBLE: illegal operation argument format:"
						 arg))))
			  (expression-operation-arguments exp))))
        (lambda ()
	    (apply proc (map (lambda (arg-proc) (arg-proc))
			     arg-procs)))))


(define (primitive-operation-lookup ops name)
    (let ((record (assoc name ops)))
        (if record
	    (cadr record)
	    (error "ASSEMBLE: No such primitive operation named:"
		   name))))
