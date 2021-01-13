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



(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
            (accumulate op initial (cdr seq)))))


(define (map proc seq)
    (accumulate (lambda (x y)
                    (cons (proc x) y))
                nil
                seq))


(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))


(define (filter predicate seq)
    (accumulate (lambda (x y)
                    (if (predicate x)
                        (cons x y)
                        y))
                nil
                seq))



(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low
              (enumerate-interval (+ low 1)
                                  high))))



(define (unique-number-sequences max length)
    (if (<= length 1)
        (map (lambda (num) (list num))
             (enumerate-interval 1 max))
        (flatmap (lambda (i)
                     (map (lambda (j) (cons i j))
                          (unique-number-sequences (- i 1)
                                                   (- length 1))))
                 (enumerate-interval length max))))


(define (triple-sum triple)
    (+ (car triple)
       (cadr triple)
       (caddr triple)))


(define (enumerate-triples n s)
    (filter (lambda (triple)
                (= (triple-sum triple) s))
            (unique-number-sequences n 3)))
