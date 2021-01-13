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

(define (length seq)
    (if (null? seq)
        0
        (+ 1 (length (cdr seq)))))




(define (ripple-carry-adder seq-a seq-b seq-s c-out)
    (define (install-full-adder seq-a seq-b seq-s c-out)
        (if (null? seq-a)
            (let ((c (make-wire)))
                (set-signal! c 0)
                c)
            (begin (full-adder (car seq-a)
                               (car seq-b)
                               (install-full-adder (cdr seq-a)
                                                   (cdr seq-b)
                                                   (cdr seq-s)
                                                   (make-wire))
                               (car seq-s)
                               c-out)
                   c-out)))

    (if (and (= (length seq-a) (length seq-b))
             (= (length seq-b) (length seq-s)))
        (begin (install-full-adder seq-a seq-b seq-s c-out)
               'installed)
        (error "RIPPLE-CARRY-ADDER: Sequences are not of same length"
               (list (list "A:" (length seq-a))
                     (list "B:"  (length seq-b))
                     (list "S:"  (length seq-s))))))
