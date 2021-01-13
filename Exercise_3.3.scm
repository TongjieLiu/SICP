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
    (define (withdraw amount)
        (if (< amount balance)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient balance"))

     (define (deposit amount)
         (set! balance (+ balance amount))
         balance)

     (define (dispatch password-in-use message)
         (if (meq? password-in-use password)
             (cond ((meq? message 'withdraw) withdraw)
                   ((meq? message 'deposit) deposit)
                   (else "Unknown request -- ACCOUNT(Object)"))
             "Incorrect password"))

     dispatch)
