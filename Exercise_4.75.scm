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

(define (unique-query exps) (car exps))


(define (uniquely-asserted operands frame-stream)
    (stream-flatmap (lambda (frame)
		        (let ((output-stream (qeval (unique-query operands)
						    (singleton-stream frame))))
			    (if (and (not (stream-null? output-stream))
				     (stream-null? (stream-cdr output-stream)))
				(singleton-stream output-stream)
				the-empty-stream)))
		    frame-stream))


(put 'unique 'qeval uniquely-asserted)
