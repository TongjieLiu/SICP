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


(define (average x y) (/ (+ x y) 2))

(define (midpoint-segment seg)
    (make-point (average (x-point (start-point seg))
                         (x-point (end-point seg)))
                (average (y-point (start-point seg))
                         (y-point (end-point seg)))))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))
