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

;a
; In afterfib-n-2, there are two instuctions:
;     (assign n (reg val))
;     (restore val)
; since their only purpose is to prepare the 
; controller for adding old "val" and new "val"
; together:
;     (assign val (op +) (reg val) (reg n))
; we can simplify them by a single instruction:
;     (restore n)
; then "n" will contain the value of old "val,
; and "val" itself will be kept unchanged, the
; goal will still be fulfilled.




;b
(define (make-save inst machine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg (get-register machine reg-name)))
	    (lambda ()
	        (push stack
		      (cons reg-name
			    (get-contents reg)))
		(advance-ps ps)))))


(define (make-restore inst machine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg (get-register machine reg-name)))
	    (lambda ()
	        (let ((pair (pop stack)))
		    (if (eq? (car pair)
			     reg-name)
			(begin (set-contents! reg
					      (cdr pair))
			       (advance-pc pc))
			(error
			    "Restore into a different register --RESTORE"
			    reg-name)))))))




;c
(define (make-new-machine)
    (let ((pc (make-register 'pc))
	  (flag (make-register 'flag))
	  (stack '())
	  (the-instuction-sequence '()))
        (let ((the-ops (list (list 'initialize-stack
                                   (lambda ()
				       (map (lambda (s)
					        (s 'initialize))
					    stack)))))
	      (register-table (list (list 'pc pc)
				    (list 'flag flag))))


            (define (allocate-register name)
	        (if (assoc name register-table)
		    (error "Multiply defined register: "
			   name)
		    (begin (set! register-table
			         (cons (list name
					     (make-register name))
				       register-table))
			   (set! stack
			         (cons (list name (make-stack))
				       stack))))

		'register-allocated)


	    (define (lookup-register name)
	        (let ((val (assoc name register-table)))
		    (if val
			(cadr val)
			(error "Unknown register: "
			       name))))


	    (define (execute)
	        (let ((insts (get-contents pc)))
		    (if (null? insts)
			'done
			(begin ((instruction-execution-proc (car insts)))
			       (execute)))))


	    (define (dispatch message)
	        (cond ((eq? message 'start)
		          (set-contents! pc the-instuction-sequence)
			  (execute))
		      ((eq? message 'install-instuction-sequence)
		          (lambda (seq)
			      (set! the-instruction-sequence seq)))
		      ((eq? message 'allocate-register)
		          allocate-register)
		      ((eq? message 'get-register)
		          lookup-register)
		      ((eq? message 'install-operations)
		          (lambda (ops)
			      (set! the-ops (append the-ops ops))))
		      ((eq? message 'stack) stack)
		      ((eq? message 'operations) the-ops)
		      (else (error "Unknown request --MACHINE"
				   message))))


	    dispatch)))



(define (make-save inst machine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg (get-register machine reg-name)))
	    (lambda ()
	        (let ((record (assoc reg-name stack)))
		    (if record
			(begin (push (cadr record)
			             (get-contents reg))
                               (advance-pc pc))
			(error "Unknown register" reg-name)))))))


(define (make-restore inst macine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg (get-register machine
				 reg-name)))
	    (lambda ()
	        (let ((record (assoc reg-name stack)))
		    (if record
			(begin (set-contents! reg
                                              (pop (cadr record)))
			       (advance-pc pc))
			(error "Unknown register" reg-name)))))))
