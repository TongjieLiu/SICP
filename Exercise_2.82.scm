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

(define nil '())



(define true #t)
(define false #f)



(define (attach-tag type-tag x)
    (cons type-tag x))


(define (type-tag x) (car x))


(define (contents x) (cdr x))



(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
            (accumulate op initial (cdr seq)))))


(define (map proc seq)
    (accumulate (lambda (item result)
                    (cons (proc item) result))
                nil
                seq))


(define (filter predicate seq)
    (accumulate (lambda (item result)
                    (if (predicate item)
                        (cons item result)
                        result))
                nil
                seq))



(define (reverse seq)
    (define (iter old new)
        (if (null? old)
            new
            (iter (cdr old)
                  (cons (car old) new))))

    (iter seq nil))



(define (unique-symbols symbols)
    (if (null? symbols)
        nil
        (let ((duplicates (filter (lambda (item)
                                          (eq? item (car symbols)))
                                  (cdr symbols))))
            (if (null? duplicates)
                (cons (car symbols)
                      (unique-symbols (cdr symbols)))
                (unique-symbols (cdr symbols))))))



(define (coerce-to-type args type)
    (define (iter olds news)
        (if (null? olds)
            (reverse news)
            (let ((old->new (get-coercion (type-tag (car olds))
                                          type)))
                (if old->new
                    (cons (old->new (car olds)) news)
                    false))))

    (iter args nil))


(define (coerce-args types args)
    (define (try-each-type unique-types)
        (if (null? unique-types)
            false
            (let ((new-args (coerce-to-type args (car unique-types))))
                (if new-args
                    new-args
                    (try-each-type (cdr unique-types))))))

    (try-each-type (unique-symbols types)))


(define (apply-generic op . args)
    (let ((types (map type-tag args)))
        (let ((proc (get op types)))
            (if proc
                (apply proc (map contents args))
                (let ((new-args (coerce-args types args)))
                    (if new-args
                        (apply apply-generic (cons op new-args))
                        (error "No procedure is available for the given types -- APPLY-GENERIC"
                               types)))))))
