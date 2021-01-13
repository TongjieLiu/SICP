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

(define (merge-weighted a b weight)
    (cond ((stream-null? a) b)
          ((stream-null? b) a)
          (else (let ((car1 (stream-car a))
                      (car2 (stream-car b)))
                    (cond ((< (weight car1) (weight car2))
                              (cons-stream car1
                                           (merge-weighted (stream-cdr a)
                                                           b
                                                           weight)))
                          ((< (weight car2) (weight car1))
                              (cons-stream car2
                                           (merge-weighted a
                                                           (stream-cdr b)
                                                           weight)))
                          (else
                              (cons-stream car1
                                           (merge-weighted (stream-cdr a)
                                                           (stream-cdr b)
                                                           weight))))))))



(define (weighted-pairs a b weight)
    (cons-stream (list (stream-car a)
                       (stream-car b))
                 (merge-weighted (stream-map (lambda (item)
                                                 (list (stream-car a)
                                                       item))
                                             (stream-cdr b))
                                 (weighted-pairs (stream-cdr a)
                                                 (stream-cdr b))
                                 weight)))




(define a (weighted-pairs integers
                          integers
                          (lambda (pair)
                              (+ (car pair)
                                 (cadr pair)))))




(define (divisible? n d) (= (remainder n d) 0))



(define stream-i (stream-filter (lambda (n)
                                    (and (not (divisible? n 2))
                                         (not (divisible? n 3))
                                         (not (divisible? n 5))))
                                integers))


(define b (weighted-pairs stream-i
                          stream-i
                          (lambda (pair)
                              (let ((i (car pair))
                                    (j (cadr pair)))
                                  (+ (* 2 i)
                                     (* 3 j)
                                     (* 5 i j))))))
