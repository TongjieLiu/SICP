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

(load "Exercise_5.43.scm")




(define SUPPORTED-OPEN-CODING-OPERATIONS '(= * - +))
(define GENERALIZED-OPEN-CODING-OPERATIONS '(+ *))




(define (member item alist)
    (cond ((null? alist) #f)
	  ((equal? item (car alist)) #t)
	  (else (member item (cdr alist)))))


(define (length alist)
    (if (null? alist)
	0
	(+ 1 (length (cdr alist)))))




(define (application-operand-number operands)
    (length operands))




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
	  ((and (application? exp)
		(let ((op (application-operator exp)))
		    (and (member op
                                 SUPPORTED-OPEN-CODING-OPERATIONS)
			 (not (find-variable ct-env op)))))
	      (if (member (application-operator exp)
			  GENERALIZED-OPEN-CODING-OPERATIONS)
		  (compile-generalized-open-coding exp
						   target
						   linkage
						   ct-env)
		  (if (= (application-operand-number
			     (application-operands exp)) 2)
	              (compile-open-coding exp target linkage ct-env)
		      (error "COMPILE: unexpected number of arguments"
			     exp))))
	  ((application? exp)
	      (compile-application exp target linkage ct-env))
	  (else (error "COMPILE: unexpected type of expression"
		       exp))))


(define (spread-arguments args op-iseq ct-env)
    (instruction-sequence-preserve
        '(env continue)
        (compile (application-first-operand args)
		 'arg1
		 'next
		 ct-env)
	(instruction-sequence-preserve
	    '(arg1 env continue)
	    (compile (application-first-operand
	                 (application-rest-operands args))
		     'arg2
		     'next
		     ct-env)
	    op-iseq)))


(define (compile-open-coding exp target linkage ct-env)
    (attach-linkage
        linkage
	(spread-arguments
	    (application-operands exp)
            (make-instruction-sequence
                '(arg1 arg2)
	        '(,target)
	        `((assign ,target (op ,(application-operator exp))
		                      (reg arg1) (reg arg2))))
	    ct-env)))


(define (spread-first-argument args op-iseq ct-env)
    (instruction-sequence-preserve
	'(arg1 env continue)
	(compile (application-first-operand args)
		 'arg2
		 'next
		 ct-env)
	op-iseq))


(define (compile-generalized-open-coding exp target linkage ct-env)
    (define (iter operator operands)
        (if (application-no-operand? operands)
	    (make-instruction-sequence
	        '(arg1)
		`(,target)
		`((assign ,target (reg arg1))))
	    (instruction-sequence-append
                (spread-first-argument
	            operands
                    (make-instruction-sequence
                        '(arg1 arg2)
	                '(arg1)
	                `((assign arg1 (op ,operator)
		                           (reg arg1) (reg arg2))))
		    ct-env)
		(iter operator
		      (application-rest-operands operands)))))



    (let ((operator (application-operator exp))
	  (operands (application-operands exp)))
        (let ((n (application-operand-number operands)))
            (cond ((= n 1)
		      (error "COMPILE: unexpected number of operands"
			     exp))
	          ((= n 2)
     	              (compile-open-coding exp target linkage ct-env))
                  (else
	              (attach-linkage linkage
                          (instruction-sequence-append
                              (compile-open-coding exp 'arg1 'next ct-env)
	                      (iter operator 
		                    (application-rest-operands
			                (application-rest-operands operands))))))))))
