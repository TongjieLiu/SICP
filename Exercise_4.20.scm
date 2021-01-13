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

(define (letrec-bindings exp) (cadr exp))

(define (letrec-body exp) (cddr exp))

(define (letrec-formal-parameters bindings)
    (map (lambda (binding) (car binding))
         bindings))

(define (letrec-arguments bindings)
    (map (lambda (binding) (cadr binding))
         bindings))



(define (make-let parameters arguments body)
    (cons 'let
          (cons (map (lambda (p a) (list p a))
                     parameters
                     arguments)
                body)))



(define (make-combination procedure . arguments)
    (cons procedure arguments))



(define (letrec->let exp)
    (let ((bindings (lecrec-bindings exp)))
        (let ((parameters (letrec-formal-parameters bindings))
              (arguments (letrec-arguments bindings)))
            (make-let parameters
                      (map (lambda (p) '*unassigned*)
                           parameters)
                      (append (map (lambda (p a)
                                       (make-combination 'set!
                                                         p
                                                         a))
                                   parameters
                                   arguments)
                              (letrec-body exp))))))
