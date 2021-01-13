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

(define (pairs a b)
    (let ((car1 (stream-car a))
          (car2 (stream-car b))
          (cdr1 (stream-cdr a))
          (cdr2 (stream-cdr b)))
        (cons-stream (list car1 car2)
                     (interleave (interleave (stream-map (lambda (item)
                                                             (list car1 item))
                                                          cdr2)
                                             (stream-map (lambda (item)
                                                             (list item car2))
                                                         cdr1))
                                 (pairs cdr1 cdr2)))))
