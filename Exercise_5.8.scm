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

(define (lookup-label labels label-name)
    (let ((val (assoc label-name labels)))
        (if val
	    (cdr val)
	    #f)))




(define (extract-labels text receive)
    (if (null? text)
	(receive '() '())
	(extract-labels (cdr text)
			(lambda (rest-insts labels)
			    (let ((inst (car text)))
			        (if (symbol? inst)
                                    (if (lookup-label labels inst)
                                        (error "Duplicate label -- ASSEMBLE"
					       inst)
					(receive insts
						 (cons (make-label-entry inst
									 insts)
						       labels)))
				    (receive (cons (make-instruction inst)
						   insts)
					     labels)))))))
