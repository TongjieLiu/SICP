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

(define (member? x items)
    (cond ((null? items) #f)
	  ((equal? x (car items)) #t)
	  (else (member? x (cdr items)))))


(define (distinct? . vars)
    (define (iter items)
        (cond ((null? items) #t)
	      ((null? (cdr items)) #t)
	      ((member? (car items) (cdr items)) #f)
	      (else (iter (cdr items)))))

    (iter vars))



(define (same-distance? var items col-distance) ; row distance = col distance?
    (cond ((null? items) #f)
	  ((= (abs (- var (car items))) col-distance) #t)
	  (else (same-distance? var (cdr items) (+ col-distance 1)))))



(define (diagonal? . vars)
    (define (iter items)
        (cond ((null? items) #f)
	      ((same-distance? (car items) (cdr items) 1) #t)
	      (else (iter (cdr items)))))

    (iter vars))



(define (eight-queens)
    (let ((col1 (amb 1 2 3 4 5 6 7 8))
	  (col2 (amb 1 2 3 4 5 6 7 8))
	  (col3 (amb 1 2 3 4 5 6 7 8))
	  (col4 (amb 1 2 3 4 5 6 7 8))
	  (col5 (amb 1 2 3 4 5 6 7 8))
	  (col6 (amb 1 2 3 4 5 6 7 8))
	  (col7 (amb 1 2 3 4 5 6 7 8))
	  (col8 (amb 1 2 3 4 5 6 7 8)))
        (require (distinct? col1
			    col2
			    col3
			    col4
			    col5
			    col6
			    col7
			    col8))
	(require (not (diagonal? col1
			         col2
			         col3
			         col4
			         col5
			         col6
			         col7
			         col8)))
        (list (list "Column 1" col1)
	      (list "Column 2" col2)
	      (list "Column 3" col3)
	      (list "Column 4" col4)
	      (list "Column 5" col5)
	      (list "Column 6" col6)
	      (list "Column 7" col7)
	      (list "Column 8" col8))))
