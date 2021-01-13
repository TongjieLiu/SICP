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

(define (delay-it exp env) (list 'thunk exp env))

(define (thunk? obj) (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))


(define (memo-delay-it exp env) (list 'memo-thunk exp env))

(define (memo-thunk? obj) (tagged-list? obj 'memo-thunk))

(define (memo-thunk-exp memo-thunk) (cadr memo-thunk))

(define (memo-thunk-env memo-thunk) (caddr memo-thunk))


(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))

(define (evaluated-thunk-value evaluated-thunk) (cadr evaluated-thunk))



(define (procedure-type procedure) (car procedure))


(define (procedure-parameter-names procedure)
    (map (lambda (parameter)
             (if (pair? parameter)
                 (car parameter)
                 parameter))
         (procedure-parameters procedure)))



(define (list-of-delayed-args args env params)
    (map (lambda (arg param)
             (if (not (pair? param))
                 (actual-value arg env)
                 (let ((type (cadr param)))
                     (cond ((eq? type 'lazy)
                               (delay-it arg env))
                           ((eq? type 'memo-lazy)
                               (memo-delay-it arg env))
                           (else (error "Unknown type of argument"
                                        type))))))
         args
         params))



(define (apply procedure arguments env)
    (cond ((primitive-procedure? procedure)
              (apply-primitive-procedure procedure
                                         (list-of-values arguments)))
          ((compound-procedure? procedure)
              (eval-sequence (procedure-body procedure)
                             (extend-environment (procedure-parameter-names procedure)
                                                 (list-of-delayed-args arguments
                                                                       env
                                                                       (procedure-parameters procedure))
                                                 (procedure-environment procedure))))
         (else (error "Unknown procedure type -- APPLY"
                      (procedure-type procedure)))))



(define (force-it obj)
    (cond ((thunk? obj)
              (actual-value (thunk-exp obj)
                            (thunk-env obj)))
          ((memo-thunk? obj)
              (let ((result (actual-value (memo-thunk-exp obj)
                                          (memo-thunk-env obj))))
                  (set-car! obj 'evaluated-thunk)
                  (set-car! (cdr obj) result)
                  (set-cdr! (cdr obj) '())
                  result))
          ((evalutaed-thunk? obj) (evaluated-thunk-value obj))
          (else obj)))
