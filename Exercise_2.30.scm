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

(define t
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))


(define nil '())



(define (square x) (* x x))


(define (square-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree))
              (square tree))
          (else
              (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))



(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))


(define (square-tree-mr tree)
    (map (lambda (subtree)
             (if (pair? subtree)
                 (square-tree subtree)
                 (square subtree)))
         tree))
