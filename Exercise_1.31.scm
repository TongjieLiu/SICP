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

(define (product x ubound term next)
    (define (product-iter x result)
        (if (> x ubound)
            result
            (product-iter (next x)
                          (* result (term x)))))

    (product-iter x 1))


(define (inc x) (+ x 1))


(define (factorial n)
    (product 1 n * inc))


(define (pi-fourth n)
    (define (pi-fourth-term x)
        (/ (* (- x 1) (+ x 1))
           (* x x)))

    (define (add2 x) (+ x 2))

    (product 3.0
             (+ 3 (* (- n 1) 2))
             pi-fourth-term
             add2))
