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

(define (cont-frac n d k)
    (define (level i)
        (if (> i k)
            0
            (/ (n i) (+ (d i) (level (+ i 1))))))

    (level 1))


(define (e-est times)
    (define (e-d i)
        (let ((r (remainder i 3)))
            (if (= r 2)
                (* 2 (+ 1 (/ (- i r) 3.0)))
                1.0)))

    (define (iter n)
        (cond ((<= n times)
                  (display n)
                  (display ": ")
                  (display (+ 2
                              (cont-frac (lambda (i) 1.0)
                                         e-d
                                         n)))
                  (newline)
                  (iter (+ n 1)))))

    (iter 1))
