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

(define (reverse seq)
    (define (iter seq result)
        (if (null? seq)
            result
            (iter (cdr seq)
                  (cons (car seq)
                        result))))

    (iter seq '()))




(define (list-of-values exps env)
    (define (iter exps result)
        (if (no-operands? exps)
            result
            (iter (rest-operands exps)
                  (cons (eval (first-operand exps)
                              env)
                        result))))

    (reverse (iter exps '())))
