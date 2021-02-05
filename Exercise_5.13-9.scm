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

; fibonacci-machine controller instruction sequence
(define test-seq
    '(start
        (assign n (op read))
	(assign continue (label fib-done))
	(perform (op stack-initialize))
      fib-loop
        (test (op <=) (reg n) (const 1))
	(branch (label base-case))

	(save continue)
	(assign continue (label after-first-fib))
	(save n)
	(assign n (op -) (reg n) (const 1))
	(goto (label fib-loop))
      after-first-fib
        (restore n)
	(assign n (op -) (reg n) (const 2))
	(save val)
	(assign continue (label after-second-fib))
	(goto (label fib-loop))
      after-second-fib
        (restore n)
	(assign val (op +) (reg val) (reg n))
	(restore continue)
	(goto (reg continue))
      base-case
        (assign val (reg n))
	(goto (reg continue))
      fib-done
        (perform (op print) (reg val))
        (perform (op stack-print-statistics))))



; factorial-machine controller instruction sequence
(define test-seq2
    '(start
        (assign n (op read))
	(assign continue (label fact-done))
	(perform (op stack-initialize))
      fact-loop
        (test (op =) (reg n) (const 1))
	(branch (label base-case))

	(save n)
	(assign n (op -) (reg n) (const 1))
	(save continue)
	(assign continue (label after-fact))
	(goto (label fact-loop))
      after-fact
        (restore continue)
	(restore n)
	(assign val (op *) (reg n) (reg val))
	(goto (reg continue))
      base-case
        (assign val (const 1))
	(goto (reg continue))
      fact-done
        (perform (op print) (reg val))
	(perform (op stack-print-statistics))
	(goto (label start))))




; fundamental utilities
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
			  (error "REGISTER: unassigned register"
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
				(else (error "REGISTER TRACE: unexpected message" m)))))
		  (else (error "REGISTER: unexpected message"
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
	  (flag (make-register 'flag)))
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
		       (list 'read
			     (lambda ()
			         (display "INPUT: ")
				 (read)))
		       (list 'print
			     (lambda messages
			         (display "OUTPUT: ")
				 (apply printn messages)))
		       (list 'stack-initialize
		             (lambda ()
			         (stack-initialize machine-stack)))
		       (list 'stack-print-statistics
			     (lambda ()
			         (stack-print-statistics machine-stack)))))
	       (machine-register-table
		   (list (list 'pc pc)
		         (list 'flag flag))))


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
			(error "MACHINE: No such register named"
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
				    (else (error "GET INSTRUCTION COUNT: unexpected message"
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
		      (else (error "MACHINE: unexpected message" m))))


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
    ; the machine. For this reason we have to allocate them when we turn
    ; the register trace "on" if we want to do this before all runnings of
    ; the machine start.
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
            (error "LABEL: no such label named"
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
				     (else (error "ASSEMBLE: illegal instruction error"
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
		     (error "ASSEMBLE: illegal instruction format"
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
				    ((expression-label? arg)
				        (expression-label-analyze arg labels))
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
	    (error "ASSEMBLE: No such primitive operation named"
		   name))))
