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

(load "Exercise_5.39.scm") ; lexical address representation
(load "Exercise_5.40.scm") ; compile-time environment representation




(define (find-variable ct-env var)
    (define (find-frame ct-env fnum)
        (define (do-find-variable vars fnum dnum)
	    (cond ((null? vars)
		      (find-frame (ct-environment-get-enclosing-environment
				      ct-env)
				  (+ fnum 1)))
		  ((eq? var (car vars))
		      (make-lexical-address fnum dnum))
		  (else (do-find-variable (cdr vars)
					  fnum
					  (+ dnum 1)))))

	(if (ct-environment-null? ct-env)
	    #f
	    (let ((ct-frame (ct-environment-get-first-frame ct-env)))
	        (do-find-variable ct-frame fnum 0))))


    (find-frame ct-env 0))
