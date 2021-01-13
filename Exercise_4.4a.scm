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

(define (and-exp? exp)
    (tagged-list? exp 'and))


(define (predicates exp) (cdr exp))


(define (first-predicate predicates)
    (car predicates))


(define (rest-predicates predicates)
    (cdr predicates))


(define (no-predicate? predicates)
    (null? predicates))



(define (eval-and exp env)
    (define (iter ps last-value)
        (if (no-predicate? ps)
            last-value
            (let ((value (eval (first-predicate ps)
                               env)))
                (if (true? value)
                    (iter (rest-predicates ps)
                          value)
                    false))))

    (iter (predicates exp) true))




(define (or-exp? exp)
    (tagged-list? exp 'or))



(define (eval-or exp env)
    (define (iter ps)
        (if (no-predicate? ps)
            false
            (let ((value (eval (first-predicate ps)
                               env)))
                (if (true? value)
                    value
                    (iter (rest-predicates ps))))))

    (iter (predicates exp)))
