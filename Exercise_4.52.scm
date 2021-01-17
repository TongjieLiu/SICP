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

(define (if-fail? exp) (tagged-list exp 'if-fail))

(define (if-fail-exp1 exp) (cadr exp))

(define (if-fail-exp2 exp) (caddr exp))


(define (analyze-if-fail exp)
    (let ((exp1 (analyze (if-fail-exp1 exp)))
	  (exp2 (analyze (if-fail-exp2 exp))))
        (lambda (env succeed fail)
	    (exp1 env
		  succeed
		  (lambda ()
		      (exp2 env
			    succeed
			    fail))))))
