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

(define (square x) (* x x))


(define (cube x) (* x x x))


(define (cubic a b c)
    (lambda (x)
        (+ (cube x)
           (* a (square x))
           (* b x)
           c)))


(define (abs x)
    (if (>= x 0)
        x
        (- x)))



(define (fixed-point f guess)
    (define (close-enough? a b)
        (< (abs (- a b)) 0.0001))

    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? next guess)
                next
                (try next))))

    (try guess))
            


(define dx 0.00001)


(define (deriv g)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x))
           dx)))


(define (newton-transform g)
    (lambda (x)
        (- x
           (/ (g x) ((deriv g) x)))))


(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))
