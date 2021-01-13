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

(define (front-dpair deque) (car deque))


(define (rear-dpair deque) (cdr deque))


(define (set-front-ptr! deque dpair)
    (set-car! deque dpair))


(define (set-rear-ptr! deque dpair)
    (set-cdr! deque dpair))




(define (make-dpair prev-dpair item next-dpair)
    (cons prev-dpair (cons item next-dpair)))



(define (item-dpair dpair)
    (car (cdr dpair)))


(define (previous-dpair dpair)
    (car dpair))


(define (next-dpair dpair)
    (cdr (cdr dpair)))



(define (set-previous-dpair! dpair previous-dpair)
    (set-car! dpair previous-dpair))


(define (set-next-dpair! dpair next-dpair)
    (set-cdr! (cdr dpair) next-dpair))




(define (make-deque) (cons '() '()))



(define (empty-deque? deque)
    (null? (front-dpair deque)))


(define (front-deque deque)
    (if (empty-deque? deque)
        (error "FRONT-QUEUE: Empty deque")
        (item-dpair (front-dpair deque))))


(define (rear-deque deque)
    (if (empty-deque? deque)
        (error "REAR-DEQUE: Empty deque")
        (item-dpair (rear-dpair deque))))



(define (front-insert-deque! deque item)
    (if (empty-deque? deque)
        (let ((new-dpair (make-dpair '()
                                     item
                                     '())))
            (set-front-ptr! deque new-dpair)
            (set-rear-ptr! deque new-dpair)
            'done)
        (let ((new-dpair (make-dpair '()
                                     item
                                     (front-dpair deque))))
            (set-previous-dpair! (front-dpair deque)
                                 new-dpair)
            (set-front-ptr! deque new-dpair)
            'done)))


(define (rear-insert-deque! deque item)
    (if (empty-deque? deque)
        (let ((new-dpair (make-dpair '()
                                     item
                                     '())))
            (set-front-ptr! deque new-dpair)
            (set-rear-ptr! deque new-dpair)
            'done)
        (let ((new-dpair (make-dpair (rear-dpair deque)
                                     item
                                     '())))
            (set-next-dpair! (rear-dpair deque)
                             new-dpair)
            (set-rear-ptr! deque new-dpair)
            'done)))


(define (front-delete-deque! deque)
    (if (empty-deque? deque)
        (error "FRONT-DELETE-DEQUE!: Empty deque")
        (begin (set-front-ptr! deque
                               (next-dpair (front-dpair deque)))
               (if (null? (front-dpair deque))
                   (set-rear-ptr! deque '())
                   (set-previous-dpair! (front-dpair deque)
                                        '()))
               'done)))


(define (rear-delete-deque! deque)
    (if (empty-deque? deque)
        (error "REAR-DELETE-DEQUE!: Empty deque")
        (begin (set-rear-ptr! deque
                              (previous-dpair (rear-dpair deque)))
               (if (null? (rear-dpair deque))
                   (set-front-ptr! deque '())
                   (set-next-dpair! (rear-dpair deque)
                                    '()))
               'done)))



(define (print-deque deque)
    (define (iter line-num dpair)
        (if (not (null? dpair))
            (begin (display line-num)
                   (display ": ")
                   (display (item-dpair dpair))
                   (newline)
                   (iter (+ line-num 1)
                         (next-dpair dpair)))))

    (if (empty-deque? deque)
        (display "The deque is empty")
        (iter 1 (front-dpair deque))))
