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

(define x1 -3)
(define x2 3)
(define y1 -3)
(define y2 3)



(define (rand-in-range low high)
    (let ((range (- high low)))
        (+ low (random (* 1.0 range)))))


(define (square x) (* x x))


(define (abs x)
    (if (>= x 0) x (- x)))


(define (p)
    (let ((x (rand-in-range x1 x2))
          (y (rand-in-range y1 y2)))
        (<= (+ (square (- x 0))
               (square (- y 0)))
            1)))


(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
                  (/ trials-passed trials))
              ((experiment)
                  (iter (- trials-remaining 1)
                        (+ trials-passed 1)))
              (else
                  (iter (- trials-remaining 1)
                        trials-passed))))

   (iter trials 0))


(define (estimate-integral trials predicate x1 x2 y1 y2)
    (let ((area (* (abs (- x1 x2))
                   (abs (- y1 y2)))))
        (* 1.0 area (monte-carlo trials p))))



(define (test trials)
    (estimate-integral trials p x1 x2 y1 y2))
