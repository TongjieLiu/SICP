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

(define (make-combination procedure arguments)
    (cons procedure arguments))


(define (make-prodedure-definition name formal-parameters body)
    (cons 'define (cons (cons name formal-parameters) body)))


(define (make-seq . seqs) seqs)




(define (let? exp) (tagged-list? exp 'let))

(define (let-simple-type? exp) (pair? (cadr exp)))

(define (let-bindings exp)
    (let ((the-second-part (cadr exp)))
        (if (pair? the-second-part)
            the-second-part
            (caddr exp))))

(define (let-formal-parameters bindings)
    (if (null? bindings)
        '()
        (cons (car (car bindings))
              (let-formal-parameters (cdr bindings)))))

(define (let-arguments bindings)
    (if (null? bindings)
        '()
        (cons (cdr (car bindings))
              (let-arguments (cdr bindings)))))

(define (let-body exp)
    (let ((the-second-part (cadr exp)))
        (if (pair? the-second-part)
            (cddr exp)
            (cdddr exp))))


(define (let-procedure-name exp) (cadr exp))



(define (let->combination exp)
    (if (let-simple-type? exp)
        (make-combination (make-lambda (let-formal-parameters (let-bindings exp))
                                       (let-body exp))
                          (let-arguments (let-bindings exp)))
        (make-begin (make-seq (make-procedure-definition (let-procedure-name exp)
                                                         (let-formal-parameters (let-bindings exp))
                                                         (let-body exp))
                              (make-combination (let-procedure-name exp)
                                                (let-arguments (let-bindings exp)))))))
                                                         



(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp)
              (make-procedure (lambda-parameters exp)
                              (lambda-body exp)
                              env))
          ((let? exp) (eval (let->combination exp) env))
          ((begin? exp) 
              (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond->if exp) env))
          ((application? exp)
              (apply (eval (operator exp) env)
                     (list-of-values (operands exp) env)))
          (else
              (error "Unknown expression type -- EVAL" exp))))
