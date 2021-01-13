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

(define (make-mobile left right)
    (list left right))


(define (make-branch length structure)
    (list length structure))


(define (left-branch mobile)
    (car mobile))


(define (right-branch mobile)
    (car (cdr mobile)))


(define (branch-length branch)
    (car branch))


(define (branch-structure branch)
    (car (cdr branch)))



(define (total-weight x)
    (if (not (pair? x))
        x
        (+ (total-weight (branch-structure (left-branch x)))
           (total-weight (branch-structure (right-branch x))))))



(define true #t)
(define false #f)


(define (balanced? x)
    (if (not (pair? x))
        true
        (if (and (balanced? (branch-structure (left-branch x)))
                 (balanced? (branch-structure (right-branch x))))
            (= (total-weight (branch-structure (left-branch x)))
               (total-weight (branch-structure (right-branch x))))
            false)))
