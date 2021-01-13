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

(define (even? n)
    (= (remainder n 2) 0))


(define (same-parity first . tail)
    (define (extract tail take?)
        (cond ((null? tail) '())
              ((take? (car tail))
                  (cons (car tail)
                        (extract (cdr tail)
                                 take?)))
              (else
                  (extract (cdr tail)
                           take?))))
            
    (if (even? first)
        (cons first
              (extract tail
                       even?))
        (cons first
              (extract tail
                       (lambda (n) (not (even? n))))))) 
