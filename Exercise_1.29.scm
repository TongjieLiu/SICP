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

(define (cube x) (* x x x))


(define (sum n b c next-c term next)
    (if (> n b)
        0
        (+ (* c (term n))
           (sum (next n) b (next-c c) next-c term next))))


(define (integral f a b n)
    (define (integral-c c)
        (cond ((= c 2) 4)
              ((= c 4) 2)
              ((= c 1) 4)))

    (define (simpsons-rule h)
        (define (integral-next a) (+ a h))
        
        (* (/ h 3)
           (+ (sum a (- b h) 1 integral-c f integral-next) 
              (f b))))

    (simpsons-rule (/ (- b a) n)))
