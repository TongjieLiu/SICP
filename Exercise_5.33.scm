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

; INTERFACES:
;     1. COMPILE
;     2. PRINT-STATEMENTS
;
;
; Exercise_5.33:
;     Differences: 1. "factorial" evaluates the argument "n" first in the
;                  alternative expression of "if" expression, but "facto-
;                  rial-alt" evaluates "(factorial (- n 1))" first.
;                  2. Because of difference #1, "factorial-alt" needs to
;                  save register "env" one more time than  "factorial" in
;                  the argument evaluation process mentioned above.
;                  3. Because of difference #1, "factorial" needs to save
;                  register "argl" one more time than "factorial-alt" in
;                  the argument evaluation process mentioned above.
;     Efficiency: They are the same in this matter.


(define fact-rec
'(define (factorial n)
    (if (= n 1)
	1
	(* (factorial (- n 1)) n))))


(define fact-rec-alt
'(define (factorial-alt n)
    (if (= n 1)
	1
	(* n (factorial-alt (- n 1))))))


(define fact-iter
'(define (factorial n)
    (define (iter product counter)
        (if (> counter n)
	    product
	    (iter (* counter product)
		  (+ counter 1))))

    (iter 1 1)))




; basic tools
(define (map proc items)
    (if (null? items)
	'()
	(cons (proc (car items))
	      (map proc (cdr items)))))


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


(define (reverse items)
    (define (iter items result)
        (if (null? items)
	    result
	    (iter (cdr items)
		  (cons (car items) result))))

    (iter items '()))


(define (memq item alist)
    (cond ((null? alist) #f)
	  ((eq? item (car alist)) item)
	  (else (memq item (cdr alist)))))


(define (list-union a b)
    (cond ((null? a) b)
	  ((memq (car a) b)
	      (list-union (cdr a) b))
	  (else (cons (car a)
		      (list-union (cdr a) b)))))


(define (list-difference a b)
    (cond ((null? a) '())
	  ((memq (car a) b)
	      (list-difference (cdr a) b))
	  (else (cons (car a)
		      (list-difference (cdr a) b)))))


(define (tagged-list? alist type-tag)
    (and (pair? alist)
	 (let ((head (car alist)))
	     (and (symbol? head)
	          (eq? head type-tag)))))




; the compiler
(define ALL-REGISTERS '(proc argl env continue val))



(define (make-compiler-label-generator)
    (let ((label-number 0))
        (lambda (prefix)
	    (let ((n label-number))
	        (set! label-number
		      (+ label-number 1))
		(string->symbol
		    (string-append (symbol->string prefix)
				   (number->string n)))))))


(define make-compiler-label (make-compiler-label-generator))



; compiler interface #1
(define (compile exp target linkage)
    (cond ((self-evaluating? exp)
	      (compile-self-evaluating exp target linkage))
	  ((variable? exp)
	      (compile-variable exp target linkage))
	  ((quoted? exp)
	      (compile-quoted exp target linkage))
	  ((assignment? exp)
	      (compile-assignment exp target linkage))
	  ((definition? exp)
	      (compile-definition exp target linkage))
	  ((if? exp)
	      (compile-if exp target linkage))
	  ((cond? exp)
	      (compile-if (cond->if exp) target linkage))
	  ((begin? exp)
	      (compile-sequence (begin-expressions exp)
				target
				linkage))
	  ((lambda? exp)
	      (compile-lambda exp target linkage))
	  ((application? exp)
	      (compile-application exp target linkage))
	  (else (error "COMPILE: unexpected type of expression"
		       exp))))



(define (compile-linkage linkage)
    (cond ((eq? linkage 'return)
	      (make-instruction-sequence
		  '(continue)
		  '()
		  '((goto (reg continue)))))
	  ((eq? linkage 'next)
	      the-empty-instruction-sequence)
	  (else (make-instruction-sequence
		    '()
		    '()
		    `((goto (label ,linkage)))))))



(define (attach-linkage linkage iseq)
    (instruction-sequence-preserve '(continue)
				   iseq
				   (compile-linkage linkage)))



(define (compile-self-evaluating exp target linkage)
    (attach-linkage linkage
		    (make-instruction-sequence
		        '()
			`(,target)
			`((assign ,target (const ,exp))))))



(define (compile-variable exp target linkage)
    (attach-linkage linkage
		    (make-instruction-sequence
		        '(env)
			`(,target)
			`((assign ,target (op environment-lookup-binding)
				              (reg env) (const ,exp))))))



(define (compile-quoted exp target linkage)
    (attach-linkage linkage
		    (make-instruction-sequence
		        '()
			`(,target)
			`((assign ,target (const ,(quoted-text exp)))))))



(define (compile-assignment exp target linkage)
    (attach-linkage linkage
		    (instruction-sequence-preserve
		        '(env)
			(compile (assignment-value-expression exp)
				 'val
				 'next)
			(make-instruction-sequence
			    '(env)
			    `(,target)
			    `((perform (op environment-modify-binding)
				           (reg env)
					   (const ,(assignment-variable exp))
					   (reg val))
			      (assign ,target (reg val)))))))



(define (compile-definition exp target linkage)
    (attach-linkage linkage
		    (instruction-sequence-preserve
		        '(env)
			(compile (definition-value-expression exp)
				 'val
				 'next)
			(make-instruction-sequence
			    '(env)
			    `(,target)
			    `((perform (op environment-add-binding)
				           (reg env)
					   (const ,(definition-variable exp))
					   (reg val))
			      (assign ,target (reg val)))))))



(define (compile-if exp target linkage)
    (let ((clabel (make-compiler-label 'if-consequent))
	  (alabel (make-compiler-label 'if-alternative))
	  (dlabel (make-compiler-label 'if-done)))
        (let ((clinkage
		 (if (eq? linkage 'next)
                     dlabel
		     linkage)))
	    (let ((piseq (compile (if-predicate exp)
			          'val
			          'next))
	          (ciseq (compile (if-consequent-expression exp)
			          target
			          clinkage))
      	          (aiseq (compile (if-alternative-expression exp)
			          target
			          linkage)))
	        (instruction-sequence-preserve
	            '(env continue)
		    piseq
		    (instruction-sequence-append
		        (make-instruction-sequence
		            '(val)
			    '()
			    `((test (op false?) (reg val))
			      (branch (label ,alabel))))
		        (instruction-sequence-parallel
		            (instruction-sequence-append clabel ciseq)
			    (instruction-sequence-append alabel aiseq))
		        dlabel))))))



(define (compile-sequence exp target linkage)
    (if (sequence-last-expression? exp)
	(compile (sequence-first-expression exp) target linkage)
	(instruction-sequence-preserve
	    '(env continue)
	    (compile (sequence-first-expression exp)
                     target
		     'next)
	    (compile-sequence (sequence-rest-expressions exp)
			      target
			      linkage))))



(define (compile-lambda exp target linkage)
    (let ((proc-entry-label (make-compiler-label 'procedure-entry-point))
	  (lambda-done-label (make-compiler-label 'lambda-done)))
        (let ((first-part-linkage (if (eq? linkage 'next)
				      lambda-done-label
				      linkage)))
	    (instruction-sequence-append
	        (instruction-sequence-tack
		    (attach-linkage first-part-linkage
		                    (make-instruction-sequence
		                        '(env)
			                `(,target)
					;     the book forgets that "operation subexpression"
					; do not support "label" type operand, and here we
					; add an instruction to temporarily store this label
					; in a register and then use this register as operand,
					; avoids explicitly using a label as operand.
			                `((assign ,target (label ,proc-entry-label))
					  (assign ,target
				            (op make-compiled-procedure)
				                (reg ,target)
					        (reg env)))))
		    (compile-procedure-body exp proc-entry-label))
		lambda-done-label))))


(define (compile-procedure-body exp proc-entry-label)
    (instruction-sequence-append
        (make-instruction-sequence
	    '(env proc argl)
	    '(env)
	    `(,proc-entry-label
	      (assign env (op compiled-procedure-environment)
		              (reg proc))
	      (assign env (op environment-extend)
		              (reg env)
			      (const ,(lambda-formal-parameters exp))
			      (reg argl))))
	(compile-sequence (lambda-body exp)
		          'val
		          'return)))



(define (compile-application exp target linkage)
    (instruction-sequence-preserve
        '(env continue)
	(compile (application-operator exp) 'proc 'next)
	(instruction-sequence-preserve
	    '(proc continue)
            (compile-application-argument-list (application-operands exp))
	    (compile-application-apply target linkage))))


(define (compile-application-argument-list operands)
    (let ((args (reverse operands)))
        (if (application-no-operand? args)
	    (make-instruction-sequence
	        '()
		'(argl)
		'((assign argl (const ()))))
	    (instruction-sequence-preserve
	        '(env)
		(instruction-sequence-append
		    (compile (application-first-operand args) 'val 'next)
		    (make-instruction-sequence
		        '(val)
			'(argl)
			'((assign argl (op list) (reg val)))))
		(compile-application-argument-list-continue
		    (application-rest-operands args))))))


(define (compile-application-argument-list-continue args)
    (if (application-no-operand? args)
	the-empty-instruction-sequence
	(instruction-sequence-preserve
	    '(env) ;     "the-empty-instruction-sequence" needs no register,
	           ; therefore "env" will never be "saved" in evaluating any
	           ; final argument.
	    (instruction-sequence-preserve
	        '(argl)
		(compile (application-first-operand args) 'val 'next)
		(make-instruction-sequence
		    '(argl val)
		    '(argl)
		    '((assign argl (op cons) (reg val) (reg argl)))))
	    (compile-application-argument-list-continue
	        (application-rest-operands args)))))


(define (compile-application-apply target linkage)
    (let ((primitive-procedure-label (make-compiler-label 'apply-primitive-procedure))
	  (compiled-procedure-label (make-compiler-label 'apply-compiled-procedure))
	  (done-label (make-compiler-label 'apply-done)))
        (let ((compiled-procedure-linkage (if (eq? linkage 'next)
					      done-label
					      linkage)))
            (instruction-sequence-append
	        (make-instruction-sequence
	            '(proc)
		    '()
		    `((test (op primitive-procedure?) (reg proc))
		      (branch (label ,primitive-procedure-label))))
		(instruction-sequence-parallel
		    (instruction-sequence-append
		        compiled-procedure-label
		        (compile-application-apply-compiled-procedure
		            target
		            compiled-procedure-linkage))
		    (attach-linkage
		        linkage
	                (make-instruction-sequence
	                    '(proc argl)
		            `(,target)
		            `(,primitive-procedure-label
			      (assign ,target (op primitive-procedure-implementation)
				                  (reg proc))
		              (assign ,target (op simple-apply)
				                  (reg ,target) (reg argl))))))
	        done-label))))


(define (compile-application-apply-compiled-procedure target linkage)
    (cond ((and (not (eq? target 'val)) (eq? linkage 'return))
	      (error "COMPILE: compiled procedure returns but target isn't val"))
	  ((and (eq? target 'val) (eq? linkage 'return))
	      (make-instruction-sequence
		  '(proc continue)
		  ALL-REGISTERS
		  '((assign val (op compiled-procedure-entry) (reg proc))
		    (goto (reg val)))))
	  ((and (eq? target 'val) (not (eq? linkage 'return)))
	      (make-instruction-sequence
		  '(proc)
		  ALL-REGISTERS
		  `((assign continue (label ,linkage))
		    (assign val (op compiled-procedure-entry) (reg proc))
		    (goto (reg val)))))
	  (else (let ((after-proc-ret-label
			 (make-compiler-label 'after-procedure-return)))
	            (make-instruction-sequence
		        '(proc)
		        ALL-REGISTERS
		        `((assign continue (label ,after-proc-ret-label))
			  (assign val (op compiled-procedure-entry) (reg proc))
			  (goto (reg val))
			  ,after-proc-ret-label
			  (assign ,target (reg val))
			  (goto (label ,linkage))))))))




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



(define (begin? exp) (tagged-list? exp 'begin))


(define (begin-expressions exp) (cdr exp))


(define (make-begin seq) (cons 'begin seq))



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
			(error
			    "COND: the else clause is not at the last"))
		    (make-if (cond-clause-predicate first)
			     (sequence->exp (cond-clause-expressions first))
			     (iter rest))))))

    (iter (cond-clauses exp)))




; compiled procedure representation
(define (make-compiled-procedure proc-entry def-env)
    (list 'compiled-procedure proc-entry def-env))



(define (compiled-procedure-entry cproc) (cadr cproc))


(define (compiled-procedure-environment cproc) (caddr cproc))




; instruction sequence representation
(define (make-instruction-sequence needed-regs modified-regs statements)
    (list needed-regs modified-regs statements))


(define the-empty-instruction-sequence
    (make-instruction-sequence '() '() '()))



(define (instruction-sequence-registers-needed iseq)
    (if (symbol? iseq)
        '()
	(car iseq)))


(define (instruction-sequence-registers-modified iseq)
    (if (symbol? iseq)
        '()
	(cadr iseq)))


(define (instruction-sequence-statements iseq)
    (if (symbol? iseq)
        (list iseq)
	(caddr iseq)))



(define (instruction-sequence-register-needed? iseq reg)
    (memq reg (instruction-sequence-registers-needed iseq)))


(define (instruction-sequence-register-modified? iseq reg)
    (memq reg (instruction-sequence-registers-modified iseq)))



; combining operations of instruction sequences
(define (instruction-sequence-simple-append iseq1 iseq2)
    (make-instruction-sequence
        (list-union (instruction-sequence-registers-needed iseq1)
		    (list-difference
		        (instruction-sequence-registers-needed iseq2)
			(instruction-sequence-registers-modified iseq1)))
	(list-union (instruction-sequence-registers-modified iseq1)
		    (instruction-sequence-registers-modified iseq2))
	(append (instruction-sequence-statements iseq1)
		(instruction-sequence-statements iseq2))))


(define (instruction-sequence-append . iseqs)
    (if (null? iseqs)
        the-empty-instruction-sequence
	(instruction-sequence-simple-append
	    (car iseqs)
	    (apply instruction-sequence-append (cdr iseqs)))))


(define (instruction-sequence-preserve regs iseq1 iseq2)
    (if (null? regs)
	(instruction-sequence-append iseq1 iseq2)
	(let ((r (car regs)))
	    (if (and (instruction-sequence-register-modified? iseq1
							      r)
		     (instruction-sequence-register-needed? iseq2
							    r))
		(instruction-sequence-preserve
		    (cdr regs)
		    (make-instruction-sequence
		        (list-union
			    (list r)
		            (instruction-sequence-registers-needed iseq1))
			(list-difference
			    (instruction-sequence-registers-modified iseq1)
			    (list r))
			(cons `(save ,r)
			      (append (instruction-sequence-statements iseq1)
				      `((restore ,r)))))
		    iseq2)
		(instruction-sequence-preserve
		    (cdr regs)
		    iseq1
		    iseq2)))))


(define (instruction-sequence-tack executed non-executed)
   (make-instruction-sequence
       (instruction-sequence-registers-needed executed)
       (instruction-sequence-registers-modified executed)
       (append (instruction-sequence-statements executed)
	       (instruction-sequence-statements non-executed))))


(define (instruction-sequence-parallel iseq1 iseq2)
    (make-instruction-sequence
        (list-union (instruction-sequence-registers-needed iseq1)
		    (instruction-sequence-registers-needed iseq2))
	(list-union (instruction-sequence-registers-modified iseq1)
		    (instruction-sequence-registers-modified iseq2))
	(append (instruction-sequence-statements iseq1)
		(instruction-sequence-statements iseq2))))



; compiler interface #2
(define (print-statements iseq)
    (for-each (lambda (item)
		  (if (not (symbol? item))
		      (display "\t"))
		  (display item)
		  (newline))
	      (instruction-sequence-statements iseq)))
