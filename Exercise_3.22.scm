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

(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '()))
        (define (empty-queue?)
            (null? front-ptr))

        (define (front-queue)
            (if (empty-queue?)
                (error "FRONT: Empty queue"
                       (print-queue))
                (car front-ptr)))

        (define (insert-queue! item)
            (let ((new-pair (cons item '())))
                (if (empty-queue?)
                    (begin (set! front-ptr new-pair)
                           (set! rear-ptr new-pair)
                           dispatch)
                    (begin (set-cdr! rear-ptr new-pair)
                           (set! rear-ptr new-pair)
                           dispatch))))

        (define (delete-queue!)
            (if (empty-queue?)
                (error "DELETE!: Empty queue"
                       (print-queue))
                (begin (set! front-ptr (cdr front-ptr))
                       dispatch)))

       (define (print-queue) front-ptr)

       (define (dispatch m)
           (cond ((eq? m 'empty-queue?) empty-queue?)
                 ((eq? m 'front-queue) front-queue)
                 ((eq? m 'insert-queue!) insert-queue!)
                 ((eq? m 'delete-queue!) delete-queue!)
                 ((eq? m 'print-queue) print-queue)
                 (else (error "QUEUE(DATA OBJECT): Unknown Operation"
                              m))))

       dispatch))


    (define (empty-queue? queue) ((queue 'empty-queue?)))


    (define (front-queue queue) ((queue 'front-queue)))


    (define (insert-queue! queue item)
        ((queue 'insert-queue!) item))


    (define (delete-queue! queue)
        ((queue 'delete-queue!)))


    (define (print-queue queue) ((queue 'print-queue)))
