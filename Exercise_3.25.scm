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

(define (make-same? accept-error)
    (lambda (a b)
        (<= (abs (- a b)) accept-error)))


(define test-same? (make-same? 10))




(define (length l)
    (if (null? l)
        0
        (+ 1 (length (cdr l)))))




(define (make-table dimension same-key?)
    (let ((local-table (list '*table*)))
        (define (assoc key subtables)
            (cond ((null? subtables) false)
                  ((same-key? key (caar subtables))
                      (car subtables))
                  (else (assoc key (cdr subtables)))))


        (define (lookup keys)
            (define (iter keys table)
                (let ((subtable (assoc (car keys)
                                       (cdr table))))
                    (if subtable
                        (if (null? (cdr keys))
                            (cdr subtable)
                            (iter (cdr keys) subtable))
                        false)))

            (cond ((null? keys)
                      (error "LOOKUP: Empty key list"))
                  ((> (length keys) dimension)
                      (error "LOOKUP: Too many keys are passed"
                             keys))
                  (else (iter keys local-table)))) ; at least 1 key


        (define (insert! keys value)
            (define (do-insert! keys value table)
                (let ((key (car keys))
                      (rest-keys (cdr keys)))
                    (let ((subtable (assoc key
                                           (cdr table))))
                        (if subtable
                            (if (null? rest-keys) ; update
                                (begin (set-cdr! subtable value)
                                       'updated)
                                (do-insert! rest-keys
                                            value
                                            subtable))
                            (if (null? rest-keys) ; create
                                (begin (set-cdr! table
                                                 (cons (cons key value)
                                                       (cdr table)))
                                       table)
                                (begin (set-cdr! table 
                                                 (cons (do-insert! rest-keys
                                                                   value
                                                                   (list key))
                                                       (cdr table)))
                                       table))))))

            (cond ((null? keys)
                      (error "INSERT!: Empty key list"))
                  ((> (length keys) dimension)
                      (error "INSERT!: Too many keys are passed"
                             keys))
                  ((eq? (do-insert! keys value local-table)
                       'updated)
                      'updated)
                  (else 'done)))


            (define (dispatch m)
                (cond ((eq? m 'lookup-proc) lookup)
                      ((eq? m 'insert-proc!) insert!)
                      (else (error "TABLE(DATA OBJECT): Unknown operations"
                                   m))))


            dispatch))



(define t (make-table 5 test-same?))


(define lookup (t 'lookup-proc))


(define insert! (t 'insert-proc!))
