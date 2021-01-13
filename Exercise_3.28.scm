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

(define (logical-or a b)
    (or a b))


(define (or-gate in-1 in-2 out)
    (define (action)
        (let ((new (logical-or (get-signal in-1)
                               (get-signal in-2))))
            (after-delay or-gate-delay
                         (lambda ()
                             (set-signal! out new)))))

    (add-action! in-1 action)
    (add-action! in-2 action)
    'installed)
