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

(define (make-variable-definition var val)
    (list 'define var val))



(define (make-combination procedure . arguments)
    (cons procedure arguments))



(define (no-exp? exps) (null? exps))




(define (scan-out-defines2 body)
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
        (let ((vars (map (lambda (definition)
                             (definition-variable definition))
                         (car definitions-and-rest)))
              (vals (map (lambda (definition)
                             (definition-value definition))
                         (car definitions-and-rest)))
              (rest-of-body (cdr definitions-and-rest)))
            (append (map (lambda (var)
                             (make-variable-definition var
                                                       '*unassigned*))
                         vars)
                    (append (map (lambda (var val)
                                     (make-combination 'set!
                                                       var
                                                       val))
                                 vars
                                 vals)
                            rest-of-body)))))
