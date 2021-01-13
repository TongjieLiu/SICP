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

; implementing and & or expressions as derived expressions
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
    (define (and->if exp)
        (let ((ps (predicates exp)))
            (if (no-predicate? ps)
                true
                (expand-predicates ps)))) ; at least 1 predicate

    (define (expand-predicates ps)
        (let ((first (first-predicate ps))
              (rest (rest-predicates ps)))
            (if (no-predicate? rest)
                first
                (make-if first
                         (expand-predicates rest)
                         false))))

    (eval (and->if exp) env))


(define (or-exp? exp)
    (tagged-list? exp 'or))



(define (eval-or exp env)
    (define (or->if exp)
        (let ((ps (predicates exp)))
            (if (no-predicate? ps)
                false
                (expand-predicates ps)))) ; at least 1 predicate

    (define (expand-predicates ps)
        (let ((first (first-predicate ps))
              (rest (rest-predicates ps)))
            (if (no-predicate? rest)
                first
                (make-if first
                         first
                         (expand-predicates rest)))))

    (eval (or->if exp) env))
