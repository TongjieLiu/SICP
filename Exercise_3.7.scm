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

(define (meq? message string)
    (and (symbol? message)
         (eq? message string)))



(define (make-account balance password)
    (define (number-of-users)
        (if (pair? password)
            2
            1))

    (define (add-user new-password)
        (if (= (number-of-users) 1)
            (set! password (cons password new-password))
            (error "There is no room for another user -- ADD-USER")))

    (define (withdraw amount)
        (if (< amount balance)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient balance"))

     (define (deposit amount)
         (set! balance (+ balance amount))
         balance)

     (define (dispatch password-in-use message)
         (define (do-dispatch)
             (cond ((meq? message 'withdraw) withdraw)
                   ((meq? message 'deposit) deposit)
                   ((meq? message 'number-of-users) (number-of-users))
                   ((meq? message 'add-user) add-user)
                   (else "Unknown request -- ACCOUNT(Object)")))

         (if (pair? password)
             (if (or (meq? password-in-use (car password))
                     (meq? password-in-use (cdr password)))
                 (do-dispatch)
                 "Incorrect password")
             (if (meq? password-in-use password)
                 (do-dispatch)
                 "Incorrect password")))

     dispatch)

(define (make-joint account old-password new-password)
    ((account old-password 'add-user) new-password)
    account)
