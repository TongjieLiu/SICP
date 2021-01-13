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

(define (make-frame vars vals)
    (if (= (length vars) (length vals))
        (map (lambda (var val)
                 (cons var val))
             vars
             vals)
        (error "Specified variables and values do not match in number")))


(define (add-binding! var val frame)
    (set-cdr! frame (cons (car frame)
                          (cdr frame)))
    (set-car? frame (cons var val)))



(define the-empty-environment '())


(define (first-frame env) (car env))


(define (enclosing-environment env) (cdr env))




(define (extend-environment vars vals env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) env)
        (error "Specified variables and values do not match in number")))



(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan bindings)
            (cond ((null? bindings)
                      (env-loop (enclosing-environment env)))
                  ((eq? var (caar bindings))
                      (cdar bindings))
                  (else (scan (cdr bindings)))))

        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (scan (first-frame env))))


    (env-loop env))



(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan bindings)
            (cond ((null? bindings)
                      (env-loop (enclosing-environment env)))
                  ((eq? var (caar bindings))
                      (set-cdr! (car bindings) val))
                  (else (scan (cdr bindings)))))

        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (scan (first-frame env))))


    (env-loop env))



(define (define-variable! var val env)
    (define (scan bindings)
        (cond ((null? bindings)
                  (add-binding! var val (first-frame env))
              ((eq? var (caar bindings))
                  (set-cdr! (car bindings) val))
              (else (scan (cdr bindings))))))

    (scan (first-frame env)))
