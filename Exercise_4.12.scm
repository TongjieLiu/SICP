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

(define (find-binding end-op match-op var env)
    (define (env-loop env)
        (define (scan vars vals frame)
            (cond ((null? vars)
                      (end-op env-loop frame env))
                  ((eq? var (car vars))
                      (match-op vals))
                  (else (scan (cdr vars)
                              (cdr vals)
                              frame))))

        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)
                      frame))))

    (env-loop env))



(define (lookup-variable-value var env)
    (find-binding (lambda (env-loop frame env)
                      (env-loop (enclosing-environment env)))
                  (lambda (value-pair)
                      (car value-pair))
                  var
                  env))


(define (set-variable-value! var val env)
    (find-binding (lambda (env-loop frame env)
                      (env-loop (enclosing-environment env)))
                  (lambda (value-pair)
                      (set-car! value-pair val))
                  var
                  env))


(define (define-variable! var val env)
    (find-binding (lambda (env-loop frame env)
                      (add-binding-to-frame! var val frame))
                  (lambda (value-pair)
                      (set-car! value-pair val))
                  var
                  env))
