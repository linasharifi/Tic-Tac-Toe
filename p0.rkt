#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 

; Check whether a list is a valid board
(define (board? lst)
  (define (contains? lst element)
    (not (empty? (member element lst))))
  
  (define size (sqrt (length lst)))
  
  (cond
    [(not (integer? size)) #f]
    [(not (= (* size size) (length lst))) #f]  
    [(equal? (car lst) 'X) #t]  
    [(contains? lst '(X O E)) #t]  
    [(= 1 (abs (- (count 'X lst) (count 'O lst)))) #f]
    [else #f]))


;;; From the board, calculate who is making a move this turn
(define (next-player board)
  (define (count-helper player lst)
    (if (empty? lst)
        0
        (+ (if (equal? (car lst) player) 1 0)
           (count-helper player (cdr lst)))))

  (cond
    [(board? board)
     (if (> (count-helper 'X board) (count-helper 'O board))
         'O
         'X)]))
;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
(define (valid-move? board row col player)
  
  (define (contains? lst element)
    (not (empty? (member element lst))))

  (define size (sqrt (length board)))

  (define (get-index row col)
    (+ (* row size) col))

  (cond
    [(not (list? board)) #f]
    [(not (number? row)) #f]
    [(not (number? col)) #f]
    [(not (or (equal? player 'X) (equal? player 'O))) #f]
    [(not (contains? board 'X)) #f]
    [(not (contains? board 'O)) #f]
    [(not (> (get-index row col) 0)) #f]
    [(not (< (get-index row col) (length board))) #f]
    [(not (equal? (list-ref board (get-index row col)) 'E)) #t]
    [else #t]))

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)

  (define (get-index row col)
    (+ (* row 3) col))
    
  (cond
    [(not (list? board)) #f]
    [(not (exact-nonnegative-integer? row)) #f]
    [(not (exact-nonnegative-integer? col)) #f]
    [(not (or (equal? player 'X) (equal? player 'O))) #f]
    [else (cons player board)]))

;;; To determine whether there is a winner?
(define (winner? board)
  
  (define (contains? lst element)
    (not (empty? (member element lst))))

  (define (get-rows-k board)
    (define (h b)
      (if (empty? b)
          '()
          (cons (take b (sqrt (length board))) (h (drop b (sqrt (length board)))))))
    (h board))

  (define (diag-contains-pattern? pattern board)
    (equal? (diagonal board) pattern))

  (define (col-contains-pattern? pattern board)
    (any-col-contains? pattern (get-cols-k board)))

  (define (row-contains-pattern? pattern board)
    (any-row-contains? pattern (get-rows-k board)))

  (define (diagonal board)
    (define size (sqrt (length board)))
    (define (get-ith l)
      (if (empty? l)
          '()
          (cons (list-ref board (+ (first l) (* (first l) size)))
                (get-ith (rest l)))))
    (get-ith (range size)))

  (define (get-cols-k board)
    (if (not (list? (car board)))
        '() 
        (apply map list board)))

  (define (any-col-contains? pattern cols)
    (if (empty? cols)
        #f
        (if (equal? (car cols) pattern)
            #t
            (any-col-contains? pattern (cdr cols)))))

  (define (any-row-contains? pattern rows)
    (if (empty? rows)
        #f
        (if (equal? (car rows) pattern)
            #t
            (any-row-contains? pattern (cdr rows)))))

  (cond
    [(not (list? board)) #f]
    [(not (contains? board '(X O))) #f]
    [(or (row-contains-pattern? '(X X X) board)
         (diag-contains-pattern? '(X X X) board)
         (col-contains-pattern? '(X X X) board)) 'X]
    [(or (row-contains-pattern? '(O O O) board)
         (diag-contains-pattern? '(O O O) board)
         (col-contains-pattern? '(O O O) board)) 'O]
    [else #f]))



;;; The board is the list containing E O X
;;; Player will always be 'O
;;; returns a pair of x and y
 (define (calculate-next-move board player)
    'todo)

