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

(define nil '())


(define (map proc items)
    (accumulate (lambda (x y)
                    (cons (proc x)
                          y))
                nil
                items))


(define (accumulate op initial items)
    (if (null? items)
        initial
        (op (car items)
            (accumulate op initial (cdr items)))))


(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (tree)
                         (if (pair? tree)
                             (count-leaves tree)
                             1))
                     tree)))


(define t (list 1 2 3 4 5 (list 6 7 8 9 10 11 12 13 (list 14 15) 16) 17 18 (list 19)))
