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

(define true #t)
(define false #f)



(define nil '())



(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
            (accumulate op initial (cdr seq)))))


(define (fold-left op initial seq)
    (define (iter result seq)
        (if (null? seq)
            result
            (iter (op (car seq)
                      result)
                  (cdr seq))))

    (iter initial seq))


(define (map proc seq)
    (accumulate (lambda (item right-sublist)
                    (cons (proc item)
                          right-sublist))
                nil
                seq))


(define (for-each action seq)
    (cond ((not (null? seq))
              (action (car seq))
              (for-each action (cdr seq)))))


(define (append seq1 seq2)
    (accumulate cons seq2 seq1))


(define (reverse seq)
    (fold-left cons nil seq))


(define (length seq)
    (accumulate (lambda (item sublist-length)
                    (+ 1 sublist-length))
                0
                seq))


(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))



(define empty-board nil)


(define (adjoin-position new col-num positions)
    (cons (list col-num new)
          positions))


(define (safe? col-num positions)
    (define (unsafe-positions k-1-positions)
        (flatmap (lambda (col)
                     (let ((k (car col))
                           (p (cadr col)))
                         (list p
                               (+ p
                                  (- col-num k))
                               (- p
                                  (- col-num k)))))
                 k-1-positions))

    (define (not-contain? position unsafe-positions)
        (cond ((null? unsafe-positions)
                  true)
              ((= position (car unsafe-positions))
                  false)
              (else
                  (not-contain? position
                                (cdr unsafe-positions)))))

    (not-contain? (car (cdr (car positions)))
                  (unsafe-positions (cdr positions))))


(define (enumerate-interval low high)
    (define (iter result high)
        (if (< high low)
            result
            (iter (cons high result)
                  (- high 1))))

    (iter nil high))


(define (print-n n string)
    (for-each (lambda (i) (display string))
              (enumerate-interval 1 n)))


(define (newline-n n)
    (for-each (lambda (i) (newline))
              (enumerate-interval 1 n)))


(define (print-more . strings)
    (for-each (lambda (string) (display string))
              strings))


(define (print-positions board-size positions)
    (define (to-seq position)
        (map (lambda (row)
                 (if (= row position)
                     "*|"
                     " |"))
             (enumerate-interval 1 board-size)))

    (define (to-board)
        (map (lambda (col)
                 (to-seq (cadr col)))
             positions))

    (define (top-to-bottom board)
        (map reverse board))

    (define (print-board row-num board)
        (cond ((null? (car board))
                  (display "\n  "))
              (else
                  (display "\n  ")
                  (print-n board-size " -")
                  (print-more "\n" row-num " ")
                  (display "|")
                  (for-each display
                            (map car board))
                  (print-board (- row-num 1)
                               (map cdr board)))))

    (print-board board-size (top-to-bottom (to-board)))
    (print-n board-size " -")
    (display "\n\n  ")
    (for-each (lambda (col-num)
                  (print-more " " col-num))
              (reverse (enumerate-interval 1 board-size))))


(define (print-board positions)
    (print-positions (length positions)
                     positions))


(define (print-positions-seq seq)
    (fold-left (lambda (positions num)
                   (newline)
                   (display "Solution ")
                   (display num)
                   (print-board positions)
                   (newline-n 4)
                   (+ num 1))
               1
               seq))


(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter (lambda (positions)
                        (safe? k positions))
                    (flatmap (lambda (rest-of-queens)
                                 (map (lambda (new-row)
                                          (adjoin-position new-row
                                                           k
                                                           rest-of-queens))
                                      (enumerate-interval 1 board-size)))
                             (queen-cols (- k 1))))))

    (queen-cols board-size))
                                      
(define (print-queens board-size)
    (print-positions-seq (queens board-size)))
