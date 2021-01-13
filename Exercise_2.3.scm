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

(define (make-point x y) (cons x y))


(define (x-point p) (car p))


(define (y-point p) (cdr p))



(define (make-segment s e) (cons s e))


(define (start-point seg) (car seg))


(define (end-point seg) (cdr seg))



(define tolerance 0.00001)


(define (abs x) (if (>= x 0) x (- x)))


(define (fixed-point f guess)
    (define (close-enough? x y)
        (< (abs (- x y)) tolerance))

    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? next guess)
                next
                (try next))))

    (try guess))


(define (average-damp f)
    (lambda (x) (/ (+ x (f x)) 2)))


(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))


(define (square x) (* x x))


(define (length seg)
    (sqrt (+ (square (- (x-point (start-point seg))
                        (x-point (end-point seg))))
             (square (- (y-point (start-point seg))
                        (y-point (end-point seg)))))))



(define (make-rectangle upper-left lower-right)
    (cons (make-segment upper-left
                        (make-point (x-point lower-right)
                                    (y-point upper-left)))
          (make-segment upper-left
                        (make-point (x-point upper-left)
                                    (y-point lower-right)))))


(define (upper-side rec) (car rec))


(define (left-side rec) (cdr rec))


(define (perimeter rec)
    (+ (* (length (upper-side rec)) 2)
       (* (length (left-side rec)) 2)))


(define (area rec)
    (* (length (upper-side rec))
       (length (left-side rec))))
