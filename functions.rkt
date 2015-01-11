;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "snake-lib.rkt")

; a game is
; (make-game snake food nat)
; (define-struct game (snake food ticks))

; a direction is either
; - 'up
; - 'down
; - 'left
; - 'right

; a snake is
; (make-snake direction body)
; (define-struct snake (heading segments))

; a body is either 
; - (cons posn empty)
; - (cons posn body)
; x-coordinates increase from 1 to board-length (inclusive)
;   toward the right
; y-coordinates increase from 1 to board length (inclusive)
;   toward the top
; the default value for board-length is 50.

; a food is either
; - empty
; - (cons posn food)

;--- Test Structures ---

; game in progress

(define snape (make-snake 
               'down
               (list (make-posn 8 9)
                     (make-posn 8 10)
                     (make-posn 8 11))))
(define testgame (make-game 
                  snape
                  (list (make-posn 3 4))
                  5))

; ended game

(define voldemort (make-snake
                   'down
               (list (make-posn 8 12)
                     (make-posn 8 10)
                     (make-posn 8 11)
                     (make-posn 8 12))))

(define endedtestgame (make-game 
                      voldemort
                      (list (make-posn 3 4))
                      5))

; game with food being eaten

(define harry (make-snake 
               'down
               (list (make-posn 8 9)
                     (make-posn 8 10)
                     (make-posn 8 11))))

(define foodeatengame (make-game 
                      harry
                      (list (make-posn 20 30) 
                            (make-posn 21 34)
                            (make-posn 8 9)
                            (make-posn 33 40))
                      5))

; add-food : game posn -> game
; adds a morsel of food at the specified board position

(define (add-food g p)
  (make-game
   (game-snake g)
   (append (list p) (game-food g))
   (game-ticks g)))

(check-expect (add-food
               testgame
               (make-posn 5 6))
              (make-game 
                  snape
                  (list (make-posn 5 6) (make-posn 3 4) )
                  5))

(check-expect (add-food
               foodeatengame
               (make-posn 5 6))
              (make-game 
                      harry
                      (list (make-posn 5 6)
                            (make-posn 20 30) 
                            (make-posn 21 34)
                            (make-posn 8 9)
                            (make-posn 33 40))
                      5))

; change-direction : game direction -> game
; changes the direction in which the snake is traveling

(define (change-direction g d)
  (make-game
   (make-snake
    d
    (snake-segments (game-snake g)))
   (game-food g)
   (game-ticks g)))

(check-expect (change-direction testgame 'down)
              (make-game 
                  snape
                  (list (make-posn 3 4))
                  5))

(check-expect (change-direction testgame 'right)
              (make-game 
                  (make-snake
                   'right
                   (list (make-posn 8 9)
                         (make-posn 8 10)
                         (make-posn 8 11)))
                  (list (make-posn 3 4))
                  5))

; game-score : game -> nat
; computes the player's score, based on the snake's
; length and the time (ticks) taken to reach that
; length -- multiplies the snake's length by 25
; and subtracts the quotient of the elapsed ticks 
; and 20

(define (game-score g)
  (- (* 25 (length (snake-segments (game-snake g))))
     (quotient (game-ticks g) 20)))

(check-expect (game-score testgame) 75)
(check-expect (game-score foodeatengame) 75)

; game-over? : game -> boolean
; determines if the game is over based on if the snake runs
; the walls of the gameboard or itself

(define (game-over? g)
  (local [(define snake-head 
      (first (snake-segments (game-snake g))))]
    (or (< (posn-x snake-head) 1)
        (> (posn-x snake-head) 50)
        (< (posn-y snake-head) 1)
        (> (posn-y snake-head) 50)
        (member? snake-head (rest (snake-segments 
                                   (game-snake g)))))))

(check-expect (game-over? testgame) false)
(check-expect (game-over? endedtestgame) true)

; Helper function:
; remove-eaten: position list-of-positions -> list-of-positions
; remove eaten food from a list of positions

(define (remove-eaten snake-head lof)
  (cond
    [(empty? lof)                  empty]
    [(and (= (posn-x snake-head) 
             (posn-x (first lof)))
          (= (posn-y snake-head) 
             (posn-y (first lof))))      (rest lof)]
    [else                         (cons (first lof) 
                                        (remove-eaten snake-head 
                                        (rest lof)))]))

(check-expect (remove-eaten (make-posn 4 5) (list)) (list))
(check-expect (remove-eaten (first (snake-segments 
                                    (game-snake foodeatengame)))
                            (game-food foodeatengame))
              (list (make-posn 20 30) 
                    (make-posn 21 34)
                    (make-posn 33 40)))


                             
; advance-game : game -> game
; moves the game forward one step. One step increments
; the game's tick component and moves the snake, possibly
; causing it to eat and grow.

(define (advance-game g)
  (cond
    [(member? (first (snake-segments (game-snake g))) (game-food g))
     (cond
        [(symbol=? 'up (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'up
           (append
            (list (make-posn 
                   (posn-x (first (snake-segments (game-snake g))))
                   (+ 1 (posn-y (first (snake-segments (game-snake g)))))))
            (snake-segments (game-snake g))))
          (remove-eaten (first (snake-segments (game-snake g))) (game-food g))
          (+ 1 (game-ticks g)))]
        [(symbol=? 'down (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'down
           (append
            (list (make-posn 
                   (posn-x (first (snake-segments (game-snake g))))
                   (- (posn-y (first (snake-segments (game-snake g)))) 1)))
            (snake-segments (game-snake g))))
          (remove-eaten (first (snake-segments (game-snake g))) (game-food g))
          (+ 1 (game-ticks g)))]
        [(symbol=? 'left (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'left
           (append
            (list (make-posn 
                   (- (posn-x (first (snake-segments (game-snake g)))) 1)
                   (posn-y (first (snake-segments (game-snake g))))))
            (snake-segments (game-snake g))))
          (remove-eaten (first (snake-segments (game-snake g))) (game-food g))
          (+ 1 (game-ticks g)))]
        [(symbol=? 'right (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'right
           (append
            (list (make-posn 
                   (+ 1 (posn-x (first (snake-segments (game-snake g)))))
                   (posn-y (first (snake-segments (game-snake g))))))
            (snake-segments (game-snake g))))
          (remove-eaten (first (snake-segments (game-snake g))) (game-food g))
          (+ 1 (game-ticks g)))])]
     [(not (member? (first (snake-segments (game-snake g))) (game-food g)))
      (cond
        [(symbol=? 'up (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'up
           (append
            (list (make-posn 
                   (posn-x (first (snake-segments (game-snake g))))
                   (+ 1 (posn-y (first (snake-segments (game-snake g)))))))
            (reverse (rest (reverse (snake-segments (game-snake g)))))))
          (game-food g)
          (+ 1 (game-ticks g)))]
        [(symbol=? 'down (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'down
           (append
            (list (make-posn 
                   (posn-x (first (snake-segments (game-snake g))))
                   (- (posn-y (first (snake-segments (game-snake g)))) 1)))
            (reverse (rest (reverse (snake-segments (game-snake g)))))))
          (game-food g)
          (+ 1 (game-ticks g)))]
        [(symbol=? 'left (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'left
           (append
            (list (make-posn 
                   (- (posn-x (first (snake-segments (game-snake g)))) 1)
                   (posn-y (first (snake-segments (game-snake g))))))
            (reverse (rest (reverse (snake-segments (game-snake g)))))))
          (game-food g)
          (+ 1 (game-ticks g)))]
        [(symbol=? 'right (snake-heading (game-snake g)))
         (make-game
          (make-snake
           'right
           (append
            (list (make-posn 
                   (+ 1 (posn-x (first (snake-segments (game-snake g)))))
                   (posn-y (first (snake-segments (game-snake g))))))
            (reverse (rest (reverse (snake-segments (game-snake g)))))))
          (game-food g)
          (+ 1 (game-ticks g)))])]))


(check-expect (advance-game testgame)
              (make-game
               (make-snake
                'down
                (list (make-posn 8 8)
                      (make-posn 8 9)
                      (make-posn 8 10)))
                (list (make-posn 3 4))
                 6))

(check-expect (advance-game foodeatengame)
              (make-game 
               (make-snake
                'down
                (list (make-posn 8 8)
                      (make-posn 8 9)
                      (make-posn 8 10)
                      (make-posn 8 11)
                      ))
                (list (make-posn 20 30) 
                      (make-posn 21 34)
                      (make-posn 33 40))
                      6))
               
; Starting snake
(define game-start (make-game
                    (make-snake
                     'up
                     (list (make-posn 25 25)
                           (make-posn 25 24)
                           (make-posn 25 23)
                           (make-posn 25 22)))
                    (list (make-posn 15 20)
                          (make-posn 20 40)
                          (make-posn 30 30))
                    0))
; Call to play the game:

; (play-game game-start
;            advance-game
;            add-food
;            change-direction
;            game-score
;            game-over?)