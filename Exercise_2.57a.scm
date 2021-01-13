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

(define nil '())



(define (variable? x) (symbol? x))


(define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))



(define (=number? x n)
    (and (number? x)
         (= x n)))



(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
            (accumulate op initial (cdr seq)))))



(define (append list1 list2)
    (accumulate cons list2 list1))



(define (make-sum a b)
    (cond ((=number? a 0) b)
          ((=number? b 0) a)
          ((and (number? a)
                (number? b))
              (+ a b))
          (else (accumulate append
                            nil
                            (list '(+)
                                  (if (sum? a)
                                      (cdr a)
                                      (cons a nil))
                                  (if (sum? b)
                                      (cdr b)
                                      (cons b nil)))))))


(define (sum? exp)
    (and (pair? exp)
         (eq? (car exp) '+)))


(define (addend exp) (cadr exp))


(define (augend exp)
    (define (iter rest)
        (cond ((null? rest) 0)
              ((null? (cdr rest)) (car rest))
              (else (list '+
                          (car rest)
                          (iter (cdr rest))))))

    (iter (cddr exp)))



(define (make-product a b)
    (cond ((or (=number? a 0)
               (=number? b 0))
              0)
          ((=number? a 1) b)
          ((=number? b 1) a)
          ((and (number? a)
                (number? b))
              (* a b))
          (else (accumulate append
                            nil
                            (list '(*)
                                  (if (product? a)
                                      (cdr a)
                                      (cons a nil))
                                  (if (product? b)
                                      (cdr b)
                                      (cons b nil)))))))


(define (product? exp)
    (and (pair? exp)
         (eq? (car exp) '*)))


(define (multiplier exp) (cadr exp))


(define (multiplicand exp)
    (define (iter rest)
        (cond ((null? rest) 1)
              ((null? (cdr rest)) (car rest))
              (else (list '*
                           (car rest)
                           (iter (cdr rest))))))

    (iter (cddr exp)))



(define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))


(define (exponentiation? exp)
    (and (pair? exp)
         (eq? (car exp) '**)))


(define (base exp) (cadr exp))


(define (exponent exp) (caddr exp))



(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
              (if (same-variable? exp var)
                  1
                  0))
          ((sum? exp)
              (make-sum (deriv (addend exp)
                               var)
                        (deriv (augend exp)
                               var)))
          ((product? exp)
              (make-sum (make-product (multiplier exp)
                                      (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                      (multiplicand exp))))
          ((exponentiation? exp)
              (let ((b (base exp))
                    (e (exponent exp)))
                  (make-product (make-product e
                                              (make-exponentiation b
                                                               (- e 1)))
                                (deriv b var))))
          (else (error "unknown expression type -- DERIV" exp))))
