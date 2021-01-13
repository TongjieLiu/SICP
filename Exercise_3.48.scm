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

(define (new-make-account id mutex)
    (define (do-make-account balance)
        (define (withdraw amount)
            (if (<= amount balance)
                (begin (set! balance (- balance amount))
                       balance)
                (error "WITHDRAW: Insufficient funds"
                       balance)))

        (define (deposit amount)
            (set! balance (+ balance amount))
            balance)

        (define (get-id)
            (mutex 'acquire)
            (let ((new-id id))
                (set! id (+ id 1))
                (mutex 'release)
                new-id))

        (let ((s (make-serializer))
              (this-id (get-id)))
            (define (dispatch m)
                (cond ((eq? m 'withdraw) withdraw)
                      ((eq? m 'depoist) deposit)
                      ((eq? m 'balance) balance)
                      ((eq? m 'serializer) s)
                      ((eq? m 'id) this-id)
                      (else
                          (error "ACCOUNT(DATA OBJECT): Unknown request"
                                 m))))

            dispatch))

    do-make-account)


(define make-account (new-make-account 1 (make-mutex)))



(define (exchange a b)
    (let ((difference (- (a 'balance)
                         (b 'balance))))
        ((a 'withdraw) difference)
        ((b 'deposit) difference)))


(define (serialized-exchange a b)
    (if (< (a 'id) (b 'id))
        (((a 'serializer) ((b 'serializer) exchange)) a b)
        (((b 'serializer) ((a 'serializer) exchange)) a b)))
