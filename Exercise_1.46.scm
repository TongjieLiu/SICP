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

(define (iterative-improve good-enough? improve)
    (lambda (guess)
        (define (try guess)
            (let ((next (improve guess)))
                (if (good-enough? next guess)
                    next
                    (try next))))

        (try guess)))


(define tolerance 0.00001)


(define (abs x)
    (if (>= x 0)
        x
        (- x)))


(define (close-enough? a b)
    (< (abs (- a b)) tolerance))


(define (fixed-point f guess)
    ((iterative-improve close-enough?
                        f)
        guess))

(define (average-damp f)
    (lambda (x)
        (/ (+ x (f x)) 2)))


(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))
