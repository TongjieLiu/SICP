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

(define (simple-stream-map proc stream)
    (if (stream-null? stream)
        the-empty-stream
        (cons-stream (proc (stream-car stream))
                     (simple-stream-map proc
                                        (stream-cdr stream)))))


(define (stream-map proc . streams)
    (if (stream-null? (stream-car streams))
        the-empty-stream
        (cons-stream (apply proc
                            (simple-stream-map stream-car
                                               streams))
                     (apply stream-map
                            (cons-stream proc
                                         (simple-stream-map stream-cdr
                                                            streams))))))



(define ones (cons-stream 1 ones))


(define (add-streams a b)
    (stream-map + a b))


(define integers (cons-stream 1 (add-streams ones integers)))




(define (mul-streams a b)
    (stream-map * a b))


(define factorials (cons-stream 1
                                (mul-streams (add-streams ones
                                                          integers)
                                             factorials)))
