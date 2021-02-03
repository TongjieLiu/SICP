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

; we don't allocate registers when creating a new machine
(define (make-machine ops controller-text)
    (let ((machine (make-new-machine)))
        ((machine 'install-operations) ops)
	((machine 'install-instruction-sequence)
	    (assemble controller-text machine))
	machine))



;     allocating an already existed register will not report the "error",
; but simply return it. And procedure "allocate-register" is only called
; in the "make-machine" of original simulater, therefore this modification
; will not effect other parts of the system
(define (make-new-machine machine)
    (let ((pc (make-register 'pc))
	  (flag (make-register 'flag))
	  (stack (make-stack))
	  (the-instruction-sequence '()))
        (let ((the-ops (list (list 'initialize-stack
				   (lambda ()
				       (stack 'initialize)))))
	      (register-table (list (list 'pc pc)
				    (list 'flag flag))))


	    ;     this procedure contains the only modification that we
	    ; have made in the "make-new-machine"
	    (define (allocate-register name)
	        (let ((val (assoc name register-table)))
		    (if val
			(cadr val)
                        (let ((new-reg  (make-register name)))
		            (set! register-table
		                  (cons (list name new-reg)
				        register-table))
			    new-reg))))


	    (define (lookup-register name)
	        (let ((assoc name register-table))
		    (if val
			(cadr val)
			(error "Unknown register:"
			       name))))

            (define (execute)
	        (let ((insts (get-contents pc)))
		    (if (null? insts)
			'done
			(begin ((instruction-execution-proc (car insts)))
			       (execute)))))


	    (define (dispatch m)
	        (cond ((eq? m 'stack) stack)
		      ((eq? m 'operations) the-ops)
		      ((eq? m 'allocate-register) allocate-register)
		      ((eq? m 'get-register) lookup-register)
		      ((eq? m 'start)
		          (set-contents! pc the-instruction-sequence)
			  (execute))
		      ((eq? m 'install-instruction-sequence)
		          (lambda (seq)
			      (set! the-instruction-sequence
				    seq)))
		      ((eq? m 'install-operations)
		          (lambda (ops)
			      (set! the-ops (append the-ops ops))))
		      (else (error "Unknown request -- MACHINE" m))))


	    dispatch)))



;     If we want to use a register, we must initialize it first. Therefore
; each register will be used in at least one of two types of instructions,
; namely "assign" and "save" instructions. Then we only need to modify two
; instruction execution procedure generators to make sure that all registers
; will be allocated
(define (make-assign inst machine labels operations pc)
    (let ((target (allocate-register machine (assign-reg-name inst)))
	  (value-exp (assign-value-exp inst)))
        (let ((value-proc
		 (if (operation-exp? value-exp)
		     (make-operation-exp value-exp
					 machine
					 labels
					 operations)
		     (make-primitive-exp (car value-exp)
					 machine
					 labels))))
	    (lambda ()
	        (set-contents! target (value-proc))
		(advance-pc pc)))))


(define (make-save inst machine stack pc)
    (let ((reg (allocate-register machine (stack-inst-reg-name inst))))
        (lambda ()
	    (push stack (get-contents reg))
	    (advance-pc pc))))
