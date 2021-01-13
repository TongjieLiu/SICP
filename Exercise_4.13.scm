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

;     This only unbound variable in the first frame,
; since recreation after unbounding is simply an another
; "set!", and if we only unbound in the first frame
; then we've implemented an unshadowing operation
; that help us seeing an outside variable in the
; same name.
(define (unbound? exp) (tagged-list exp 'make-unbound!)

(define (unbound-name exp)
    (if (null? (cddr exp))
        (cadr exp)
        (error "Invalid make-unbound! expression"
               exp)))


(define (eval-unbound exp env)
    (define (scan vars vals name)
        (cond ((null? vars)
                  (error "Unbound variable -- MAKE-UNBOUND!"
                         name))
              ((eq? name (car vars))
                  (set-car! vars (car (cdr vars)))
                  (set-cdr! vars (cdr (cdr vars))))
              (else (scan (cdr vars) (cdr vals) name))))

    (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)
              (unbound-name exp))))
