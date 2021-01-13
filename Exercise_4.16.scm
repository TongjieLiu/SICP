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

; a
(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                      (env-loop (enclosing-environment env)))
                  ((eq? (car vars) var)
                      (let ((val (car vals)))
                          (if (eq? val '*unassigned*)
                              (error "Unbound variable" var)
                              val)))
                  (else (scan (cdr vars) (cdr vals)))))

        (if (eq? env the-empty-environment)
            (error "Unbound varible" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))


    (env-loop env))




; b
(define (make-combination procedure . arguments)
    (cons procedure arguments))


(define (make-let parameters arguments body)
    (if (not (= (length parameters) (length arguments)))
        (error "Unmatched formal parameters and arguments")
        (let ((bindings (map (lambda (p a) (list p a))
                             parameters
                             arguments)))
            (cons 'let (cons bindings body)))))


(define (no-exp? exps) (null? exps))



(define (scan-out-defines body)
    (define (separate body)
        (if (no-exp? body)
            (cons '() '())
            (let ((exp (first-exp body))
                  (last-result (separate (rest-exps body))))
                (if (definition? exp)
                    (cons (cons exp (car last-result))
                          (cdr last-result))
                    (cons (car last-result)
                          (cons exp (cdr last-result)))))))

    (let ((definitions-and-rest (separate body)))
        (let ((parameters (map (lambda (definition)
                                   (definition-variable definition))
                               (car definitions-and-rest)))
              (values (map (lambda (definition)
                               (definition-value definition))
                           (car definitions-and-rest)))
              (rest-of-body (cdr definitions-and-rest))
              (arguments (map (lambda (item) '*unassigned*)
                              (car definitions-and-rest))))
            (make-let parameters
                      arguments
                      (append (map (lambda (var val)
                                       (make-combination 'set! var val))
                                   parameters
                                   values)
                              rest-of-body)))))




; c
; 
;     "Make-procedure" is about the construction time, and
; "procedure-body" the selection time.
;     The former is better for the reason that a procedure
; need only be constructed once but called many times and
; this make it more efficient than the other one.
(define (make-procedure parameters body env)
    (list 'procedure parameters (scan-out-defines body) env))
