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

(load "Exercise_5.42.scm")




(define (sequence-no-expression? seq) (null? seq))


(define (sequence-append a b) (append a b))


(define (make-sequence . exps) exps)



(define (make-assignment var value)
    (list 'set! var value))




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
	(compile-sequence (transform-lambda-body (lambda-body exp))
		          'val
		          'return
			  (ct-environment-extend ct-env
						 (lambda-formal-parameters exp)))))



(define (scan-out-definition seq)
    (define (iter seq defs usages)
        (if (sequence-no-expression? seq)
	    (cons (reverse defs)
		  (reverse usages))
	    (let ((exp (sequence-first-expression seq)))
	        (if (definition? exp)
		    (iter (sequence-rest-expressions seq)
			  (cons exp defs)
			  usages)
		    (iter (sequence-rest-expressions seq)
			  defs
			  (cons exp usages))))))

    (iter seq '() '()))



(define (transform-lambda-body body)
    (let ((result (scan-out-definition body)))
        (let ((defs (car result))
	      (usages (cdr result)))
	    (if (null? defs)
		body
	        (let ((def-vars (map (lambda (def)
				         (definition-variable def))
				     defs))
		      (def-vals (map (lambda (def)
				         (definition-value-expression def))
				     defs))
		      (assignments (map (lambda (def)
				            (make-assignment
					        (definition-variable def)
					        (definition-value-expression def)))
				        defs)))
	            (let ((lambda-exp (make-lambda def-vars
					           (sequence-append
						       assignments
						       usages))))
                        (make-sequence
		            (make-application lambda-exp
				              (map (lambda (def)
						   ;     Our register-machine simulator
						   ; takes registers with the special sym-
						   ; bol "*unassigned*" as their value as
						   ; unassigned, and it report error about
						   ; any usages of unassigned registers.
						   ; Moreover, our compiler will make the
						   ; object program put this special symbol
						   ; in the register "val" when evaluating
						   ; the assignment value expression if here
						   ; we choose "*unassigned*" as in the book.
						       ''*scheme-unassigned*)
					           def-vars)))))))))
