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



(define (pair-in? pair seq)
    (cond ((null? seq) false)
          ((eq? pair (car seq)) true)
          (else (pair-in? pair (cdr seq)))))


(define (count-pairs x)
    (define (traverse x traversed)
        (cond ((not (pair? x)) 0)
              ((pair-in? x traversed) 0)
              (else (let ((new-traversed (cons x traversed)))
                        (+ (traverse (car x)
                                       new-traversed)
                           (traverse (cdr x)
                                       new-traversed)
                           1)))))

    (traverse x '()))


(define test '(a b c))
(set-car! (cdr test) test)


(define (incorrect-solution x)
    (if (not (pair? x))
        0
        (+ (incorrect-solution (car x))
           (incorrect-solution (cdr x))
           1)))
