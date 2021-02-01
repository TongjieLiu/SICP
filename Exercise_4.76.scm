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

(define (conjoin conjuncts frame-stream)
    (define (iter conjuncts last-frame-stream)
        (if (empty-conjunction? conjuncts)
	    frame-stream
	    (let (new-frame-stream (qeval (first-conjunct conjuncts)
			                  frame-stream))
	        
	        (iter (rest-conjuncts conjuncts)
		      (stream-flatmap (lambda (last)
		       		          (stream-flatmap
					      (lambda (new)
					       	  (let ((merged-frame 
					                   (compatible? last new)))
						      (if merged-frame
							  (singleton-stream
							      merged-frame)
							  the-empty-frame)))
				              new-frame-stream))
				      last-frame-stream)))))

    (iter (rest-conjuncts conjuncts)
	  (qeval (first-conjunct conjuncts)
		 frame-stream)))



(define (empty-frame? frame) (null? frame)


(define (first-binding frame) (car frame))


(define (rest-bindings frame) (cdr frame))



(define (compatible? f1 f2)
    (define (merge f1 f2)
        (define (iter old new)
	    (if (empty-frame? old)
		new
		(let ((binding (first-frame old)))
		    (let ((var (binding-variable binding))
			  (val (binding-value binding)))
		        (iter (rest-bindings old)  
		              (extend var val new))))))

	(iter f1 f2))


    (define (compare frame target-frame)
        (if (empty-frame frame)
	    #t
	    (let ((binding (first-binding frame)))
	        (let ((var (binding-variable binding))
		      (val (binding-value binding)))
		    (if (equal? val
		               (binding-in-frame var target-frame))
			(compare (rest-bindings frame)
				 target-frame)
			#f)))))


    (if (and (compare f1 f2)
	     (compare f2 f1))
	(merge f1 f2)
	#f))
