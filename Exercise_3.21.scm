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

(define (front-pair queue)
    (car queue))


(define (rear-pair queue)
    (cdr queue))


(define (set-front-ptr! queue pair)
    (set-car! queue pair))


(define (set-rear-ptr! queue pair)
    (set-cdr! queue pair))




(define (make-queue)
    (cons '() '()))



(define (empty-queue? queue)
    (null? (front-pair queue)))


(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT: Empty queue" (print-queue queue))
        (car (front-pair queue))))



(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (if (empty-queue? queue)
            (begin (set-front-ptr! queue new-pair)
                   (set-rear-ptr! queue new-pair)
                   queue)
            (begin (set-cdr! (rear-pair queue) new-pair)
                   (set-rear-ptr! queue new-pair)
                   queue))))


(define (delete-queue! queue)
    (if (empty-queue? queue)
        (error "DELETE!: Empty queue" (print-queue queue))
        (begin (set-front-ptr! queue (cdr (front-pair queue)))
               queue)))



(define (print-queue queue)
    (front-pair queue))
