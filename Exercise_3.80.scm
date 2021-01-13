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

(define (RLC R L C dt)
    (lambda (vc0 il0)
        (define vc (integral (delay dvc) vc0 dt))
        (define il (integral (delay dil) il0 dt))

        (define dvc (scale-stream il (- (/ 1 C))))
        (define dil (add-streams (scale-stream il (- (/ R L)))
                                 (scale-stream vc (/ 1 L))))

        (cons vc il)))



(define model (RLC 1 1 0.2 0.1))
(define streams-pair (model 10 0))
