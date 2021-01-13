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

(define (integral delayed-integrand initial dt)
    (cons-stream initial
                 (let ((integrand (force delayed-integrand)))
                     (if (stream-null? integrand)
                         the-empty-stream
                         (integral (stream-cdr integrand)
                                   (+ (* (stream-car integrand)
                                         dt)
                                      initial)
                                   dt)))))
