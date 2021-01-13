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

(define (even? n)
    (= (remainder n 2) 0))


(define (square x) (* x x))


(define (exp b n)
    (define (iter a b n)
        (cond ((= n 0) a)
              ((even? n)
                  (iter a (square b) (/ n 2)))
              (else
                  (iter (* a b) b (- n 1)))))

    (iter 1 b n))



(define (cons a b)
    (* (exp 2 a)
       (exp 3 b)))


(define (exponent n base)
    (define (iter r q a)
        (if (not (= r 0))
            a
            (iter (remainder q base) (/ q base) (+ a 1))))

    (iter (remainder n base) (/ n base) 0))


(define (car z) (exponent z 2))


(define (cdr z) (exponent z 3))
