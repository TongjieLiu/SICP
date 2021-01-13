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

(define (scale-series c stream)
    (scale-stream c stream))


(define (add-series a b)
    (add-streams a b))


(define (mul-series a b)
    (let ((car-a (stream-car a))
          (car-b (stream-car b))
          (cdr-a (stream-cdr a))
          (cdr-b (stream-cdr b)))
        (cons-stream (* car-a
                        car-b)
                     (add-series (cons-stream 0
                                              (mul-series cdr-a
                                                          cdr-b))
                                 (add-series (scale-series car-a
                                                           cdr-b)
                                             (scale-series car-b
                                                           cdr-a))))))




(define (invert-unit-series s)
    (define result (cons-stream 1
                               (mul-series (scale-series -1
                                                         (cdr s))
                                           result)))

    result)
