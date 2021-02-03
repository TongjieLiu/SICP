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

(define (member? item alist)
    (cond ((null? alist) #f)
	  ((equal? item (car alist)) #t)
	  (else (member? item (cdr alist)))))


(define (add-to-list item alist)
    (if (not (member? item alist))
	(cons item alist)))


(define (for-each action items)
    (if (not (null? items))
	(begin (action (car items))
	       (for-each action (cdr items)))))


(define (append a b)
    (if (null? a)
	b
	(cons (car a)
	      (append (cdr a) b))))


(define (print-list alist title)
    (display title)
    (display ":")
    (newline)
    (for-each (lambda (item)
		  (display item)
		  (newline))
	      alist))


(define (assoc key records)
    (cond ((null? records) #f)
	  ((equal? key (caar records))
	      (car records))
	  (else assoc key (cdr records))))



(define (make-new-machine)
    (let ((pc (make-register 'pc))
          (flag (make-register 'flag))
	  (stack (make-stack))
	  (the-instruction-sequence '())
	  (info-enabled #f)
	  (info-insts-table (list (list 'assign '())
			          (list 'test '())
			          (list 'branch '())
			          (list 'goto '())
			          (list 'save '())
			          (list 'restore '())
			          (list 'perform '())))
	  (info-goto-regs '())
	  (info-stack-regs '())
	  (info-source-table '()))
        (let ((the-ops (list (list 'initialize-stack
				   (lambda ()
				       (stack 'initialize)))))
	      (register-table (list (list 'pc pc)
				    (list 'flag flag))))


	    (define (allocate-register name)
                (if (assoc name register-table)
		    (error "Multiply defined register:" name)
		    (set! register-table
		          (cons (list name (make-register name))
                                register-table)))
		'register-allocated)


	    (define (lookup-register name)
	        (let ((val (assoc name register-table)))
		    (if val
			(cadr val)
			(error "Unknown register:" name))))


	    (define (execute)
	        (let ((insts (get-contents pc)))
		    (if (null? insts)
			'done
			(begin ((instruction-execution-proc (car insts)))
			       (execute)))))


	    (define (info-system-init)
	        (set! info-enabled #t)
		(info-insts-table-init)
		(info-goto-regs-init)
		(info-stack-regs-init)
		(info-source-table-init))


	    (define (get-info message)
	        (if (not (info-enabled))
		    (info-system-init))
		(cond ((eq? message 'instructions)
		          (for-each (lambda (type)
				        (print-list (cadr type)
						    (car type)))
				    info-insts-table))
		      ((eq? message 'entry-point-registers)
		          (print-list info-goto-regs
                                      "Registers containing entry points"))
		      ((eq? message 'stack-registers)
			  (print-list info-stack-regs
				      "Registers once saved or restored in the stack"))
		      ((eq? message 'register-value-sources)
		          (for-each (lambda (reg)
				        (print-list (cadr reg)
						    (car reg)))
				    info-source-table))
		      ((eq? message "help")
		          (print-list (list 'instructions
					    'entry-point-registers
					    'stack-registers
					    'register-value-sources)
				      "Currently supported options:"))))


	    (define (info-insts-table-init)
	        (for-each (lambda (inst)
			      (let ((text (instruction-text inst)))
				  (let ((type (car text)))
				      (let ((type-insts (assoc type info-insts-table)))
					  (if type-insts
					      (set-car! (cdr type-insts)
							(add-to-list inst
								     (cadr type-insts))))))))
                          the-instruction-sequence))


	    (define (info-goto-regs-init)
	        (let ((goto-insts (cadr (assoc 'goto info-insts-table))))
		    (for-each (lambda (inst)
				  (let ((dest (goto-dest inst)))
				      (if (register-exp? dest)
					  (let ((reg-name (register-exp-reg dest)))
					      (set! info-goto-regs
						    (add-to-list reg-name info-goto-regs))))))
			      goto-insts)))


            (define (info-stack-regs-init)
	        (let ((save-insts (cadr (assoc 'save info-insts-table)))
		      (restore-insts (cadr (assoc 'restore info-insts-table))))
		    (for-each (lambda (inst)
				  (let ((reg-name (stack-inst-reg-name inst)))
				      (set! info-stack-regs
					    (add-to-list reg-name
							 info-stack-regs))))
			      (append save-insts restore-insts))))


	    (define (info-source-table-init)
	        (let ((assign-insts (cadr (assoc 'assign info-insts-table))))
		    (for-each (lambda (inst)
				  (let ((reg-name (assign-reg-name inst))
					(source (assign-value-exp inst)))
				      (let ((reg-sources (assoc reg-name
								info-source-tables)))
					  (if reg-sources
					      (set-car! (cdr reg-sources)
							(add-to-list source
								     (cadr reg-sources)))
					      (set! info-source-table
						    (cons (list reg-name
							        (assign-value-exp inst))
							  info-source-table))))))
			      assign-insts)))


	    (define (dispatch message)
	        (cond ((eq? message 'stack) stack)
		      ((eq? message 'operations) the-ops)
		      ((eq? message 'allocate-register) allocate-register)
		      ((eq? message 'get-register) lookup-register)
		      ((eq? message 'get-info) get-info)
		      ((eq? message 'start)
		          (set-contents! pc the-instruction-sequence)
			  (execute))
		      ((eq? message 'install-instruction-sequence)
		          (lambda (seq)
			      (set! the-instruction-sequence seq)))
		      ((eq? message 'install-operations)
		          (lambda (ops)
			      (set! the-ops (append the-ops ops))))
		      (else (error "Unknown request -- MACHINE"
				   message))))


	    dispatch)))


(define (get-info machine option)
    ((machine 'get-info) option))
