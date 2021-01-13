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

(define (cond-extended-clause? clause)
    (let ((the-second-part (cadr clause)))
        (and (symbol? the-second-part)
             (eq? the-second-part '=>))))


(define (cond-action-procedure clause)
    (let ((rest (cddr clause)))
        (if (null? (cdr rest))
            (car rest)
            (error "Extended cond clause only support one procedure -- COND-ACTION-PROCEDURE"
                   (cdr rest)))))


(define (make-application procedure arguments)
    (cons procedure arguments))



(define (expand-clauses clauses)
    (if (null? clauses)
        'false ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                           clauses))
                (if (cond-extended-clause? first)
                    (make-if (cond-predicate first)
                             (make-application (cond-action-procedure first)
                                               (list (cond-predicate first)))
                             (expand-clauses rest))
                    (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))
