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

; in-1 ->- |inverter| ->a>-
;                           \
;                             ->- |and-gate| ->c>- |inverter| ->- out
;                           /
; in-2 ->- |inverter| ->b>-
;
; 0 0 -> 1 1 -> 1 -> 0
; 0 1 -> 1 0 -> 0 -> 1
; 1 0 -> 0 1 -> 0 -> 1
; 1 1 -> 0 0 -> 0 -> 1

(define (or-gate in-1 in-2 out)
    (let ((a (make-wire))
          (b (make-wire))
          (c (make-wire)))
        (inverter in-1 a)
        (inverter in-2 b)
        (and-gate a b c)
        (inverter c out)

        'installed))
