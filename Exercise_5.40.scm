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

(load "Exercise_5.33.scm") ; compiler




(define (compile exp target linkage ct-env)
    (cond ((self-evaluating? exp)
	      (compile-self-evaluating exp target linkage ct-env))
	  ((variable? exp)
	      (compile-variable exp target linkage ct-env))
	  ((quoted? exp)
	      (compile-quoted exp target linkage ct-env))
	  ((assignment? exp)
	      (compile-assignment exp target linkage ct-env))
	  ((definition? exp)
	      (compile-definition exp target linkage ct-env))
	  ((if? exp)
	      (compile-if exp target linkage ct-env))
	  ((cond? exp)
	      (compile-if (cond->if exp) target linkage ct-env))
	  ((begin? exp)
	      (compile-sequence (begin-expressions exp)
				target
				linkage
				ct-env))
	  ((lambda? exp)
	      (compile-lambda exp target linkage ct-env))
	  ((application? exp)
	      (compile-application exp target linkage ct-env))
	  (else (error "COMPILE: unexpected type of expression"
		       exp))))



(define (compile-self-evaluating exp target linkage ct-env)
    (attach-linkage linkage
		    (make-instruction-sequence
		        '()
			`(,target)
			`((assign ,target (const ,exp))))))



(define (compile-variable exp target linkage ct-env)
    (attach-linkage linkage
		    (make-instruction-sequence
		        '(env)
			`(,target)
			`((assign ,target (op environment-lookup-binding)
				              (reg env) (const ,exp))))))



(define (compile-quoted exp target linkage ct-env)
    (attach-linkage linkage
		    (make-instruction-sequence
		        '()
			`(,target)
			`((assign ,target (const ,(quoted-text exp)))))))



(define (compile-assignment exp target linkage ct-env)
    (attach-linkage linkage
		    (instruction-sequence-preserve
		        '(env)
			(compile (assignment-value-expression exp)
				 'val
				 'next
				 ct-env)
			(make-instruction-sequence
			    '(env)
			    `(,target)
			    `((perform (op environment-modify-binding)
				           (reg env)
					   (const ,(assignment-variable exp))
					   (reg val))
			      (assign ,target (reg val)))))))



(define (compile-definition exp target linkage ct-env)
    (attach-linkage linkage
		    (instruction-sequence-preserve
		        '(env)
			(compile (definition-value-expression exp)
				 'val
				 'next
				 ct-env)
			(make-instruction-sequence
			    '(env)
			    `(,target)
			    `((perform (op environment-add-binding)
				           (reg env)
					   (const ,(definition-variable exp))
					   (reg val))
			      (assign ,target (reg val)))))))



(define (compile-if exp target linkage ct-env)
    (let ((clabel (make-compiler-label 'if-consequent))
	  (alabel (make-compiler-label 'if-alternative))
	  (dlabel (make-compiler-label 'if-done)))
        (let ((clinkage
		 (if (eq? linkage 'next)
                     dlabel
		     linkage)))
	    (let ((piseq (compile (if-predicate exp)
			          'val
			          'next
				  ct-env))
	          (ciseq (compile (if-consequent-expression exp)
			          target
			          clinkage
				  ct-env))
      	          (aiseq (compile (if-alternative-expression exp)
			          target
			          linkage
				  ct-env)))
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



(define (compile-sequence exp target linkage ct-env)
    (if (sequence-last-expression? exp)
	(compile (sequence-first-expression exp) target linkage ct-env)
	(instruction-sequence-preserve
	    '(env continue)
	    (compile (sequence-first-expression exp)
                     target
		     'next
		     ct-env)
	    (compile-sequence (sequence-rest-expressions exp)
			      target
			      linkage
			      ct-env))))



(define (compile-lambda exp target linkage ct-env)
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
		    (compile-procedure-body exp proc-entry-label ct-env))
		lambda-done-label))))


(define (compile-procedure-body exp proc-entry-label ct-env)
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
		          'return
			  (ct-environment-extend ct-env
						 (lambda-formal-parameters exp)))))



(define (compile-application exp target linkage ct-env)
    (instruction-sequence-preserve
        '(env continue)
	(compile (application-operator exp) 'proc 'next ct-env)
	(instruction-sequence-preserve
	    '(proc continue)
            (compile-application-argument-list (application-operands exp) ct-env)
	    (compile-application-apply target linkage))))


(define (compile-application-argument-list operands ct-env)
    (let ((args (reverse operands)))
        (if (application-no-operand? args)
	    (make-instruction-sequence
	        '()
		'(argl)
		'((assign argl (const ()))))
	    (instruction-sequence-preserve
	        '(env)
		(instruction-sequence-append
		    (compile (application-first-operand args) 'val 'next ct-env)
		    (make-instruction-sequence
		        '(val)
			'(argl)
			'((assign argl (op list) (reg val)))))
		(compile-application-argument-list-continue
		    (application-rest-operands args)
		    ct-env)))))


(define (compile-application-argument-list-continue args ct-env)
    (if (application-no-operand? args)
	the-empty-instruction-sequence
	(instruction-sequence-preserve
	    '(env) ;     "the-empty-instruction-sequence" needs no register,
	           ; therefore "env" will never be "saved" in evaluating any
	           ; final argument.
	    (instruction-sequence-preserve
	        '(argl)
		(compile (application-first-operand args) 'val 'next ct-env)
		(make-instruction-sequence
		    '(argl val)
		    '(argl)
		    '((assign argl (op cons) (reg val) (reg argl)))))
	    (compile-application-argument-list-continue
	        (application-rest-operands args)
		ct-env))))


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
	      (error "COMPILE: compiled procedure return not with target val"))
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




; compile-time environment representation
(define the-empty-ct-environment '())


(define (ct-environment-extend ct-env vars)
    (cons vars ct-env))


(define (ct-environment-null? ct-env)
    (null? ct-env))


(define (ct-environment-get-first-frame ct-env)
    (car ct-env))


(define (ct-environment-get-enclosing-environment ct-env)
    (cdr ct-env))
