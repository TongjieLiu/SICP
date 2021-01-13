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

(define true #t)
(define false #f)


(define nil '())



(define (unique? item l)
    (if (null? l)
        true
        (if (= item (car l))
            false
            (unique? item (cdr l)))))


(define (make-set . items)
    (define (find-uniques elements)
        (if (null? elements)
            nil
            (let ((e (car elements))
                  (tail (cdr elements)))
                (if (unique? e tail)
                    (cons e
                          (find-uniques tail))
                    (find-uniques tail)))))

    (find-uniques items))



(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1)
              (append (cdr list1) list2))))


(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))


(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest
                    (map (lambda (subset)
                             (cons (car s)
                                   subset))
                         rest)))))
