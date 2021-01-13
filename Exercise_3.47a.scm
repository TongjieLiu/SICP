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

(define (make-semaphore size)
    (if (< size 1)
        (error "MAKE-SEMAPHORE: Size need to be at least 1"
               size))
    (let ((available size)
          (running-mutex (make-mutex))
          (available-mutex (make-mutex)))
        (lambda (m)
            (cond ((eq? m 'acquire)
                      (running-mutex 'acquire)
                      (available-mutex 'acquire)
                      (set! available
                            (- available 1))
                      (if (> available 0)
                          (running-mutex 'release)))
                      (available-mutex 'release)
                  ((eq? m 'release)
                      (available-mutex 'acquire)
                      (set! available
                            (+ available 1))
                      (if (> available size)
                          (error "SEMAPHORE(DATA OBJECT): There is no need to release"))
                      ;   when available > 1, we've already released the running-cell and
                      ; only the last running thread which had made available = 0(equal 1
                      ; after the addition above) didn't release the running-cell in the
                      ; acquiring process
                      (if (= available 1)
                          (running-mutex 'release))
                      (available-mutex 'release))
                  (else (error "SEMAPHORE(DATA OBJECT): Unknown operation"))))))
