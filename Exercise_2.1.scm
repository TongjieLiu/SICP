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

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))


(define (abs x)
    (if (>= x 0)
        x
        (- x)))


(define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
        (if (or (and (>= n 0) (>= d 0))
                (and (< n 0) (>= d 0)))
            (cons (/ n g) (/ d g))
            (cons (/ (- n) g) (/ (- d) g)))))


(define (numer x) (car x))
(define (denom x) (cdr x))


(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))


(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))


(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))


(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))


(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))


(define (print-rat x)
    (newline)
    (let ((n (numer x)))
        (cond ((= n 0)
                  (display "0"))
              (else
                  (display n)
                  (display "/")
                  (display (denom x))))))


(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
