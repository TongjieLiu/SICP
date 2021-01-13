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

(define (make-vect x y)
    (list x y))


(define (xcor-vect v)
    (car v))


(define (ycor-vect v)
    (cadr v))



(define (simple-add-vect v1 v2)
    (make-vect (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))


(define (simple-sub-vect v1 v2)
    (make-vect (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))


(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))


(define (fold-left-without-initial op seq)
    (define (iter result seq)
        (if (null? seq)
            result
            (iter (op result (car seq))
                  (cdr seq))))

    (if (null? seq)
        (error "Error: the operation needs at least two operands")
        (iter (car seq) (cdr seq))))


(define (binary-operation op-with-two-operands)
    (lambda operands
        (fold-left-without-initial  op-with-two-operands
                                    operands)))


(define add-vect (binary-operation simple-add-vect))
(define sub-vect (binary-operation simple-sub-vect))


(define test-vect (make-vect 2 5))



(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))


(define (origin-frame frame)
    (car frame))


(define (edge1-frame frame)
    (car (cdr frame)))


(define (edge2-frame frame)
    (car (cdr (cdr frame))))



(define (frame-coord-map frame)
    (lambda (v)
        (add-vect (origin-frame frame)
                  (scale-frame (xcor-vect v)
                               (edge1-frame frame))
                  (scale-frame (ycor-vect v)
                               (edge2-frame frame)))))



(define (make-segment start-point end-point)
    (list start-point
          end-point))


(define (start-point segment)
    (car segment))


(define (end-point segment)
    (cadr segment))
