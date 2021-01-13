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

(define (estimate-integral p x1 x2 y1 y2)
    (stream-map (lambda (possibility)
                    (* possibility
                       (* (- x2 x1)
                          (- y2 y1))))
                (monte-carlo (lambda ()
                                 (p (random-in-range x1 x2)
                                 (random-in-range y1 y2)))
                                 0
                                 0)))


(define (unit-circle-predicate x y)
    (<= (+ (square x)
           (square y))
        1))


(define pi (estimate-integral unit-circle-predicate
                              -1
                              1
                              -1
                              1))
