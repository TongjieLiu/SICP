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

(define (square x) (* x x))


(define (smallest-divisor n)
    (find-divisor n 2))


(define (divides? a b)
    (= (remainder b a) 0))


(define (next test-divisor)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2)))


(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))


(define (prime? n)
    (= (smallest-divisor n) n))


(define (report-prime n elapsed-time)
    (display " *** ")
    (display elapsed-time))


(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime n (- (runtime) start-time))))


(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))


(define (search-for-primes start end)
    (cond ((<= start end)
              (timed-prime-test start)
              (search-for-primes (+ start 1) end))))
