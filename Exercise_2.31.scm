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



(define (square x) (* x x))



(define (tree-map proc tree)
    (cond ((null? tree) nil)
          ((not (pair? tree))
              (proc tree))
          (else
              (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))


(define (square-tree tree)
    (tree-map square tree))


(define t
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))
