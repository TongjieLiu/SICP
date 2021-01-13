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



(define (divide? divisor n)
    (= (remainder n divisor) 0))


(define (smallest-divisor n)
    (define (stop? divisor n)
        (> (square divisor) n))

    (define (try divisor)
        (cond ((stop? divisor n) n)
              ((divide? divisor n) divisor)
              (else (try (+ divisor 1)))))

    (try 2))


(define (prime? n)
    (= (smallest-divisor n) n))



(define (map proc seq)
    (if (null? seq)
        nil
        (cons (proc (car seq))
              (map proc (cdr seq)))))


(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
            (accumulate op initial (cdr seq)))))


(define (filter predicate seq)
    (if (null? seq)
        nil
        (let ((item (car seq)))
            (if (predicate item)
                (cons item
                      (filter predicate (cdr seq)))
                (filter predicate (cdr seq))))))


(define (append seq1 seq2)
    (accumulate cons seq2 seq1))



(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low
              (enumerate-interval (+ low 1) high))))


(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))


(define (unique-pairs n)
    (flatmap (lambda (i)
                 (map (lambda (j) (list i j))
                      (enumerate-interval 1 (- i 1))))
             (enumerate-interval 2 n)))


(define (prime-sum? p)
    (prime? (+ (car p) (cadr p))))


(define (add-sum p)
    (let ((i (car p))
          (j (cadr p)))
        (list i j (+ i j))))


(define (prime-sum-pairs n)
    (map add-sum
         (filter prime-sum?
                 (unique-pairs n))))
