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

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          (else (let ((processor (get 'eval (car exp))))
                    (if (= processor -1) ; a new table implementation with "get" return -1 if not found
                        (apply (eval (operator exp) env)
                               (list-of-values (operands exp) env))
                        (processor exp env))))))



(put 'eval 'quote (lambda (exp env)
                      (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env)
                       (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env)))
(put 'eval 'begin (lambda (exp env)
                      (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond (lambda (exp env)
                     (eval (cond->if exp) env)))
