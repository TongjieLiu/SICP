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

(define (lexical-address-lookup env address)
    (define (find-frame env fnum)
        (if (= fnum 0)
	    (environment-get-first-frame env)
	    (find-frame (environment-get-enclosing-environment env)
			(- fnum 1))))

    (define (find-binding vars vals dnum)
        (if (= dnum 0)
	    (cons (car vars) (car vals))
	    (find-binding (cdr vars)
			  (cdr vals)
			  (- dnum 1))))

    (let ((fnum (lexical-address-frame-number address))
	  (dnum (lexical-address-displacement-number address)))
        (let ((frame (find-frame env fnum)))
	    (let ((binding (find-binding (frame-variables frame)
				         (frame-values frame)
				         dnum)))
	        (if (eq? (cdr binding) '*unassigned*)
		    (error "LEXICAL ADDRESS LOOKUP: unassigned variable:"
			   (car binding))
		    (cdr binding))))))



(define (lexical-address-set! env address new-val)
    (define (find-frame env fnum)
        (if (= fnum 0)
	    (environment-get-first-frame env)
	    (find-frame (environment-get-enclosing-environment env)
			(- fnum 1))))

    (define (modify-binding! vars vals dnum)
        (if (= dnum 0)
	    (set-car! vals new-val)
	    (modify-binding! (cdr vars)
			     (cdr vals)
			     (- dnum 1))))

    (let ((fnum (lexical-address-frame-number address))
	  (dnum (lexical-address-displacement-number address)))
        (let ((frame (find-frame env fnum)))
	    (modify-binding! (frame-variables frame)
				              (frame-values frame)
				              dnum))))




; frame representation
(define (make-frame vars vals) (cons vars vals))


(define (frame-variables frame) (car frame))


(define (frame-values frame) (cdr frame))


(define (frame-add-binding frame var val)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))




; environment representation
(define the-empty-environment '())


(define (environment-null? env) (null? env))


(define (environment-get-enclosing-environment env) (cdr env))


(define (environment-get-first-frame env) (car env))


(define (environment-add-frame env frame) (cons frame env))




; lexical address representation
(define (make-lexical-address fnum dnum)
    (cons fnum dnum))


(define (lexical-address-frame-number address)
    (car address))


(define (lexical-address-displacement-number address)
    (cdr address))
