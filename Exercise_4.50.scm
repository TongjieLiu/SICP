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

(define (rearrange items); "rearrange" will modify original list
    (define (find items n)
        (if (= n 1)
	    (let ((next (cdr items)))
	        (set-cdr! items (cdr next))
		next)
            (find (cdr items) (- n 1))))

    (define (iter last items len)
        (if (= len 0)
	    (set-cdr! last '())
            (let ((head (cons #t items)))
                (set-cdr! last
			  (find head
				(+ (random len) 1)))
                (iter (cdr last) (cdr head) (- len 1)))))

    (let ((head (list #t)))
        (iter head items (length items))
	(cdr head)))




(define (make-list-copy items)
    (map (lambda (x) x)
	 items))




(define (ramb? exp) (tagged-list exp 'ramb))


(define (ramb-choices exp) (cdr exp))



(define (analyze-ramb exp)
    (let ((choice-procs
	     (rearrange (make-list-copy (map analyze
			                     (ramb-choices exp))))))
        (lambda (env succeed fail)
	    (define (try-next choices)
	        (if (null? choices)
		    (fail)
		    ((car choices) env
				   succeed
				   (lambda ()
				       (try-next (cdr choices))))))

	    (try-next choice-procs))))
