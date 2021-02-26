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

;    The compiler is right-to-left in the evaluation of operands. We can
; modify its behavior to left-to-right by making the following changes:
(define (compile-application-argument-list operands)
   (instruction-sequence-append
       (make-instruction-sequence
	   '()
           '(argl)
           '((assign argl (const ()))))
       (do-compile-application-argument-list operands)))


(define (do-compile-application-argument-list operands)
    (if (application-no-operand? operands)
	the-empty-instruction-sequence
	(instruction-sequence-preserve
	    '(env) ;     "the-empty-instruction-sequence" needs no register,
	           ; therefore "env" will never be "saved" in evaluating any
	           ; final argument.
            (instruction-sequence-preserve
	        '(argl)
		(compile (application-first-operand operands) 'val 'next)
		(make-instruction-sequence
		    '(argl val)
		    '(argl)
		    '((assign argl (op adjoin) (reg argl) (reg val)))))
	    (do-compile-application-argument-list
	        (application-rest-operands operands)))))




;     But the new compiler needs an new list operation called "adjoin".
; The original list operation we used for constructing argument list is
; "cons",  and it doesn't have the ability to add new item to the end
; of a list.
;
;     From the implementation of "adjoin" in the book, we can see that
; it is slower than "cons". Additionally, we need to initialize "argl"
; to the empty list first.
(define (adjoin alist item)
    (append alist (list item)))
