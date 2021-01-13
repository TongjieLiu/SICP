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

(define (make-let bindings body)
    (cons 'let (cons bindings body)))




(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-bindings exp) (cadr exp))

(define (let*-first-binding bindings) (car bindings))

(define (let*-rest-bindings bindings) (cdr bindings))

(define (let*-no-binding? bindings) (null? bindings))

(define (make-bindings . bindings) bindings)

(define (let*-body exp) (cddr exp))



(define (let*->nested-lets exp)
    (define (construct-nested-lets bindings)
        (if (let*-no-binding? bindings)
            (let*-body exp)
            (make-let (make-bindings (let*-first-binding bindings))
                      (construct-nested-lets (let*-rest-bindings bindings)))))

    (construct-nested-lets (let*-bindings exp)))
