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

(load "Exercise_5.41.scm")




(define (compile-variable exp target linkage ct-env)
    (let ((lexical-address (find-variable ct-env exp)))
        (attach-linkage linkage
            (if lexical-address
		(make-instruction-sequence
		    '(env)
	            `(,target)
		    `((assign ,target (op lexical-address-lookup)
				          (reg env) (const ,lexical-address))))
		(make-instruction-sequence
		    '()
	            `(env ,target)
	            `((assign env (op environment-get-global-environment))
		      (assign ,target (op environment-lookup-binding)
				          (reg env) (const ,exp))))))))



(define (compile-assignment exp target linkage ct-env)
    (let ((lexical-address (find-variable ct-env
					  (assignment-variable exp))))
        (attach-linkage linkage
            (if lexical-address
	        (instruction-sequence-preserve
		    '(env)
	            (compile (assignment-value-expression exp)
		             'val
		             'next
			     ct-env)
		    (make-instruction-sequence
		        '(env)
			`(,target)
			`((perform (op lexical-address-set!)
				       (reg env)
				       (const ,lexical-address)
				       (reg val)))))
		(instruction-sequence-append
	            (compile (assignment-value-expression exp)
		             'val
		             'next
			     ct-env)
		    (make-instruction-sequence
		        '()
			`(env ,target)
			`((assign env (op environment-get-global-environment))
			  (perform (op environment-modify-binding)
				       (reg env)
				       (const ,(assignment-variable exp))
				       (reg val)))))))))
