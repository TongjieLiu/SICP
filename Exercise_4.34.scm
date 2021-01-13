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

;     The implementation of stream and list interfaces
; in the evaluated language:
;     (define underlying-cons cons)
;     (define underlying-car car)
;     (define underlying-cdr cdr)
;
;
;     (define (cons a b)
;         (underlying-cons (quote 'pair)
;                          (lambda (m) (m a b))))
;
;     (define (car x) ((underlying-cdr x) (lambda (a b) a)))
;
;     (define (cdr x) ((underlying-cdr x) (lambda (a b) b)))
;



(define (make-combination proc . args)
    (cons proc args))



(define (embeded-pair? obj)
    (and (pair obj)
         (eq? (car obj) 'pair)))


(define (embeded-pair-car epair)
    (actual-value (make-combination 'car epair)))


(define (embeded-pair-cdr epair)
    (actual-value (make-combination 'cdr epair)))


(define (embeded-list->pair epair) ; embeded-list must has at least 1 item
    (define (convert epair n)
        (cond ((null? epair) '())
              ((not (pair? epair)) ; embeded-list is a single pair
                  (list '. epair))
              (else
                  (if (= n 0) ; only obtains a finite number of items
                      (list '...) ; stops converting
                      (cons (embeded-pair-car epair)
                            (convert (embeded-pair-cdr epair)
                                     (- n 1)))))))

    (convert epair 10))
        
        



(define (user-print obj)
    (cond ((compound-procedure? obj)
              (display (list 'COMPOUND-PROCEDURE
                             (procedure-parameters obj)
                             (procedure-body obj)
                             '<procedure-env>)))
          ((embeded-pair? obj)
              (display (embeded-list->pair obj)))
          (else (display obj))))
