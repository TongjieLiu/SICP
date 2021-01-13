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

(define true #t)
(define false #f)


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
                            (list (if (sum? a)
                                      a
                                      (cons a nil))
                                  '(+)
                                  (if (sum? b)
                                      b
                                      (cons b nil)))))))



(define (contain? sym seq)
    (cond ((null? seq) false)
          ((eq? sym (car seq)) true)
          (else (contain? sym (cdr seq)))))



(define (sum? exp)
    (and (pair? exp)
         (contain? '+ exp)))


(define (addend exp)
    (define (extract-product e)
        (if (eq? '+ (car e))
            nil
            (cons (car e)
                  (extract-product (cdr e)))))

    (if (eq? (cadr exp) '+)
        (car exp)
        (extract-product exp)))



(define (length seq)
    (accumulate (lambda (x y) (+ y 1))
                0
                seq))



(define (memq sym seq)
    (cond ((null? seq) false)
          ((eq? sym (car seq)) seq)
          (else (memq sym (cdr seq)))))



(define (augend exp)
    (let ((rest (cdr (memq '+ exp))))
        (if (= (length rest) 1)
            (car rest)
            rest)))


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
                            (list (if (product? a)
                                      a
                                      (cons a nil))
                                  '(*)
                                  (if (product? b)
                                      b
                                      (cons b nil)))))))


(define (product? exp)
    (and (pair? exp)
         (eq? (cadr exp) '*)))


(define (multiplier exp) (car exp))


(define (multiplicand exp)
    (define (extract rest)
        (cond ((null? rest) 1)
              ((null? (cdr rest)) (car rest))
              (else rest)))

    (extract (cddr exp)))



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
          (else (error "unknown expression type -- DERIV" exp))))



(define test-exp '(x + x * 2 * x * 3 + 4 + x * x))
