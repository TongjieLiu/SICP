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

(define EXIST-DELAYED-FILTERS #f)


(define DELAYED-FILTER-LIST '())



(define (delayed-negate operands frame-stream)
    (if (all-bound? (negated-query operands)
		    frame-stream)
	(negate operands frame-stream)
	(let ((new-delayed-filter
	         (lambda (current-frame-stream)
	             (negate operands current-frame-stream))))
	    (set! DELAYED-FILTER-LIST
	          (cons new-delayed-filter
			DELAYED-FILTER-LIST))
	    (set! EXIST-DELAYED-FILTERS #t))))


(put 'not 'qeval delayed-negate)


(define (delayed-lisp-value call frame-stream)
    (if (all-bound? (args call)
		    frame-stream)
	(lisp-value call frame-stream)
	(let ((new-delayed-filter
		 (lambda (current-frame-stream)
		     (lisp-value call current-frame-stream))))
	    (set! DELAYED-FILTER-LIST
		  (cons new-delayed-filter
			DELAYED-FILTER-LIST))
	    (set! EXIST-DELAYED-FILTERS #t))))


(put 'lisp-value 'qeval delayed-lisp-value)



(define (all-bound? exp frame-stream)
    (define (extract-vars exp)
        (define (tree-walk e)
            (cond ((var? e) (list e))
		  ((pair? e)
		      (append (tree-walk (car e))
			      (tree-walk (cdr e))))
		  (else '())))

	(tree-walk exp))


    (define (all-bound-in-frame? vars frame)
        (if (null? vars)
	    #t
	    (let ((var (car vars)))
	        (let ((binding (binding-in-frame var frame)))
		    (if binding
			(all-bound-in-frame? (cdr vars) frame)
			#f)))))


    (define (iter vars stream)
        (if (stream-null? stream)
	    #t
	    (let ((frame (stream-car stream)))
	        (if (all-bound-in-frame? vars frame)
		    (iter vars (stream-cdr stream))
		    #f))))


    (iter (extract-vars exp)
	  frame-stream))



(define (simple-query pattern frame-stream)
    (let ((new-stream
	     (stream-flatmap (lambda (frame)
			         (stream-append-delayed
				     (find-assertions pattern
						      frame)
				     (delay (apply-rules pattern
							 frame))))
			     frame-stream)))
        (if EXIST-DELAYED-FILTERS
	    (try-filters new-stream)
	    new-stream)))


(define (try-filters stream)
    (define (iter filters stream)
        (if (null? filters)
	    stream
	    (let ((filter (car filters)))
	        (try-filters (cdr filters)
		             (filter stream)))))

    (let ((filters DELAYED-FILTER-LIST))
        (set! DELAYED-FILTER-LIST '())
	(set! EXIST-DELAYED-FILTERS #f)
	(iter filters
	      stream)))
