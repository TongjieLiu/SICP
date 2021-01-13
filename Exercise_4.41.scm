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

(define (map proc items)
    (if (null? items)
	'()
	(cons (proc (car items))
	      (map proc (cdr items)))))


(define (filter predicate? items)
    (cond ((null? items) '())
	  ((predicate? (car items))
	      (cons (car items)
		    (filter predicate? (cdr items))))
	  (else (filter predicate? (cdr items)))))


(define (accumulate proc init items)
    (if (null? items)
	init
	(proc (car items)
	      (accumulate proc init (cdr items)))))


(define (append a b)
    (if (null? a)
	b
	(cons (car a) (append (cdr a) b))))


(define (flatmap proc items)
    (accumulate append '() (map proc items)))



(define (member? x items)
    (cond ((null? items) #f)
	  ((equal? x (car items)) #t)
	  (else (member? x (cdr items)))))


(define (distinct? items)
    (cond ((null? items) #t)
	  ((null? (cdr items)) #t)
	  ((member? (car items) (cdr items)) #f)
	  (else (distinct? (cdr items)))))



(define (add-person new people)
    (flatmap (lambda (new)
	         (map (lambda (people)
			  (cons new people))
		      people))
	     new))



(define baker-floors (list 1 2 3 4))
(define cooper-floors (list 2 3 4))
(define fletcher-floors (list 2 3 4))
(define miller-floors (list 3 4 5))
(define smith-floors (list 1 2 3 4 5))


(define (multiple-dwelling)
    (let ((cooper-miller
	     (filter (lambda (people)
		         (let ((miller (cadr people))
			       (cooper (car people)))
			     (> miller cooper)))
                     (flatmap (lambda (cooper)
	                          (map (lambda (miller)
                                           (list cooper miller))
		                        miller-floors))
	                      cooper-floors))))
        (let ((fletcher-miller-cooper
		 (filter (lambda (people)
			     (let ((fletcher (car people))
				   (cooper (cadr people)))
			         (> (abs (- fletcher cooper)) 1)))
			 (add-person fletcher-floors cooper-miller))))
	    (let ((sfcm
		     (filter (lambda (people)
			         (let ((smith (car people))
				       (fletcher (cadr people)))
				     (> (abs (- smith fletcher)) 1)))
			     (add-person smith-floors fletcher-miller-cooper))))
	        (let ((bsfcm
		         (filter distinct?
				 (add-person baker-floors sfcm))))
		    (map (lambda (people)
			     (list (list 'baker (car people))
				   (list 'smith (cadr people))
				   (list 'fletcher (caddr people))
				   (list 'cooper (car (cdddr people)))
				   (list 'miller (car (cdr (cdddr people))))))
			 bsfcm))))))
