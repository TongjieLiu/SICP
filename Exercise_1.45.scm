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

(define tolerance 0.00001)


(define (fixed-point f guess)
    (define (close-enough? a b)
        (< (abs (- a b)) tolerance))

    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? next guess)
                next
                (try next))))

    (try guess))


(define (average-damp f)
    (lambda (x)
        (/ (+ x (f x)) 2)))


(define (compose f g)
    (lambda (x)
        (f (g x))))


(define (repeated f n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated f (- n 1)))))


(define (even? n)
    (= (remainder n 2) 0))


(define (square x) (* x x))


(define (exp b n)
    (define (iter a b n)
        (cond ((= n 0) a)
              ((even? n)
                  (iter a (square b) (/ n 2)))
              (else
                  (iter (* a b)  b (- n 1)))))

    (iter 1 b n))


(define (nth-root n)
    (lambda (x)
        (fixed-point ((repeated average-damp (- n 1))
                         (lambda (y) (/ x (exp y (- n 1)))))
                     1.0)))
