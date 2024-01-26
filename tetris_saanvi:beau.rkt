;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw7 (3) (2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;; Bin-Packing Game

;;; Constants

; the height of the board:
(define BOARD-HEIGHT 20)

; the width of the board:
(define BOARD-WIDTH 10)

; the size of one brick/space:
(define BRICK 20)

; the board for bin-packing game:
(define BOARD (empty-scene (* BOARD-WIDTH BRICK)
                           (* BOARD-HEIGHT BRICK)))

;;; Data Definitions (FINISH DATA DEFINITIONS)

; A Brick is a (make-brick Number Number Color)
(define-struct brick [x y color])

; A Pt (2D point) is a (make-posn Integer Integer)

; A Tetra is a (make-tetra Pt Bricks)
(define-struct tetra [center bricks])
; The center point is the point around which the tetra
; rotates when it spins.

; A Bricks (set-of-bricks) is one of:
; - '()
; - (cons Brick Bricks)
; unordered list of bricks or empty list

; A World is a (make-world Tetra Bricks)
(define-struct world [tetra pile])
; The set of bricks represents the pile of bricks
; at the bottom of the screen.

;;; Examples

(define O (overlay (square 20 "outline" "black") (square 20 "solid" "green")))

(define LOB-empty '())
(define LOB1 (list (make-brick 1 1 "purple")
                   (make-brick 2 1 "purple")
                   (make-brick 3 1 "purple")
                   (make-brick 3 2 "purple")))

(define LOB2 (list (make-brick 8 4 "blue")
                   (make-brick 8 5 "blue")
                   (make-brick 8 6 "blue")
                   (make-brick 8 7 "blue")))

(define LOB3 (list (make-brick 11 4 "blue")
                   (make-brick 11 5 "blue")
                   (make-brick 11 6 "blue")
                   (make-brick 11 7 "blue")))

(define LOB4 (list (make-brick 1 1 "green")
                   (make-brick 1 2 "green")
                   (make-brick 1 3 "green")
                   (make-brick 1 4 "green")))

(define tetra-ex (make-tetra (make-posn 2 1) LOB1))
(define tetra-ex2 (make-tetra (make-posn 8 5) LOB2))

(define emptyworld (make-world tetra-ex '()))
(define world1 (make-world tetra-ex LOB1))
(define toobig-world (make-world tetra-ex (list (make-brick 5 18 "blue")
                                                (make-brick 5 19 "blue")
                                                (make-brick 5 20 "blue")
                                                (make-brick 5 21 "blue"))))

(define world3 (make-world (make-tetra (make-posn 5 10) (list (make-brick 5 10 "purple")
                                                              (make-brick 4 10 "purple")
                                                              (make-brick 6 10 "purple")
                                                              (make-brick 6 11 "purple")))
                           LOB1))
(define world4 (make-world (make-tetra (make-posn 1 10) (list (make-brick 1 10 "purple")
                                                              (make-brick 0 10 "purple")
                                                              (make-brick 2 10 "purple")
                                                              (make-brick 2 11 "purple")))
                           LOB1))

;;; Tetra Functions

; make-o: Number Number -> Tetra
; Makes a O-shaped tetra with given coordinates

(define (make-o x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y "green")
                    (make-brick (+ x 1) y "green")
                    (make-brick x (+ y 1) "green")
                    (make-brick (+ x 1) (+ y 1) "green"))))

(check-expect (make-o 1 1) (make-tetra (make-posn 1 1)
                                       (list
                                        (make-brick 1 1 "green")
                                        (make-brick 2 1 "green")
                                        (make-brick 1 2 "green")
                                        (make-brick 2 2 "green"))))

(check-expect (make-o 5 5) (make-tetra (make-posn 5 5)
                                       (list (make-brick 5 5 "green")
                                             (make-brick 6 5 "green")
                                             (make-brick 5 6 "green")
                                             (make-brick 6 6 "green"))))

; make-i: Number Number -> Tetra
; Makes a I-shaped tetra with given coordinates

(define (make-i x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y "blue")
                    (make-brick (- x 1) y "blue")
                    (make-brick (- x 2) y "blue")
                    (make-brick (+ x 1) y "blue"))))

(check-expect (make-i 1 1)
  (make-tetra (make-posn 1 1) (list (make-brick  1            1 "blue")
                                    (make-brick (- 1 1)       1 "blue")
                                    (make-brick (- 1 2)       1 "blue")
                                    (make-brick (+ 1 1)       1 "blue"))))

(check-expect (make-i 5 5)
  (make-tetra (make-posn 5 5) (list (make-brick 5 5 "blue")
                                    (make-brick (- 5 1) 5 "blue")
                                    (make-brick (- 5 2) 5 "blue")
                                    (make-brick (+ 5 1) 5 "blue"))))           

; make-l: Number Number -> Tetra
; Makes a L-shaped tetra with given coordinates

(define (make-l x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y "purple")
                    (make-brick (- x 1) y "purple")
                    (make-brick (+ x 1) y "purple")
                    (make-brick (+ x 1) (+ y 1) "purple"))))

(check-expect (make-l 1 1)
  (make-tetra (make-posn 1 1) (list (make-brick 1            1 "purple")
                                    (make-brick (- 1 1)      1 "purple")
                                    (make-brick (+ 1 1)      1 "purple")
                                    (make-brick (+ 1 1) (+ 1 1) "purple"))))
(check-expect (make-l 5 5)
   (make-tetra (make-posn 5 5) (list (make-brick 5 5 "purple")
                                     (make-brick (- 5 1) 5 "purple")
                                     (make-brick (+ 5 1) 5 "purple")
                                     (make-brick (+ 5 1) (+ 5 1) "purple"))))          


; make-j: Number Number -> Tetra
; Makes a J-shaped tetra with given coordinates

(define (make-j x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y "lightblue")
                    (make-brick (- x 1) y "lightblue")
                    (make-brick (+ x 1) y "lightblue")
                    (make-brick (- x 1) (+ y 1) "lightblue"))))

(check-expect (make-j 1 1)
  (make-tetra (make-posn 1 1) (list (make-brick 1             1 "lightblue")
                                    (make-brick (- 1 1)       1 "lightblue")
                                    (make-brick (+ 1 1)       1 "lightblue")
                                    (make-brick (- 1 1)  (+ 1 1) "lightblue"))))
(check-expect (make-j 5 5)
   (make-tetra (make-posn 5 5) (list (make-brick 5 5 "lightblue")
                                     (make-brick (- 5 1) 5 "lightblue")
                                     (make-brick (+ 5 1) 5 "lightblue")
                                     (make-brick (- 5 1) (+ 5 1) "lightblue"))))           

; make-t: Number Number -> Tetra
; Makes a T-shaped tetra with given coordinates

(define (make-t x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y "orange")
                    (make-brick (- x 1) y "orange")
                    (make-brick (+ x 1) y "orange")
                    (make-brick x (+ y 1) "orange"))))

(check-expect (make-t 1 1)
  (make-tetra (make-posn 1 1) (list (make-brick 1             1 "orange")
                                    (make-brick (- 1 1)       1 "orange")
                                    (make-brick (+ 1 1)       1 "orange")
                                    (make-brick 1       (+ 1 1) "orange"))))
(check-expect (make-t 5 5)
  (make-tetra (make-posn 5 5) (list (make-brick 5 5 "orange")
                                    (make-brick (- 5 1) 5 "orange")
                                    (make-brick (+ 5 1) 5 "orange")
                                    (make-brick 5 (+ 5 1) "orange"))))           

; make-z: Number Number -> Tetra
; Makes a Z-shaped tetra with given coordinates

(define (make-z x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y "lightpink")
                    (make-brick (+ x 1) y "lightpink")
                    (make-brick x (+ y 1) "lightpink")
                    (make-brick (- x 1) (+ y 1) "lightpink"))))

(check-expect (make-z 1 1)
   (make-tetra (make-posn 1 1) (list (make-brick 1             1 "lightpink")
                                    (make-brick (+ 1 1)        1 "lightpink")
                                    (make-brick 1        (+ 1 1) "lightpink")
                                    (make-brick (- 1 1)  (+ 1 1) "lightpink"))))

(check-expect (make-z 5 5)
   (make-tetra (make-posn 5 5) (list (make-brick 5 5 "lightpink")
                                     (make-brick (+ 5 1) 5 "lightpink")
                                     (make-brick 5 (+ 5 1) "lightpink")
                                     (make-brick (- 5 1) (+ 5 1) "lightpink"))))           
              

; make-s: Number Number -> Tetra
; Makes a S-shaped tetra with given coordinates

(define (make-s x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y "red")
                    (make-brick (- x 1) y "red")
                    (make-brick x (+ y 1) "red")
                    (make-brick (+ x 1) (+ y 1) "red"))))

(check-expect (make-s 1 1)
   (make-tetra (make-posn 1 1) (list (make-brick 1             1 "red")
                                    (make-brick (- 1 1)        1 "red")
                                    (make-brick 1        (+ 1 1) "red")
                                    (make-brick (+ 1 1)  (+ 1 1) "red"))))
(check-expect (make-s 5 5)
   (make-tetra (make-posn 5 5) (list (make-brick 5 5 "red")
                                     (make-brick (- 5 1) 5 "red")
                                     (make-brick 5 (+ 5 1) "red")
                                     (make-brick (+ 5 1) (+ 5 1) "red"))))           

;;; Drawing Tetra

; place-brick: Image Number Number Image -> Image
; Places an image using coordinates of bricks rather than pixels

(define (place-brick first x y second)
  (place-image first
               (* BRICK (+ 1/2 x))
               (* BRICK (- BOARD-HEIGHT (+ 1/2 y)))
               second))

(check-expect (place-brick O 5 10 BOARD)
              (place-image O
                           (* (+ 1/2 5) BRICK)
                           (* BRICK (- BOARD-HEIGHT (+ 1/2 10)))
                           BOARD))
(check-expect (place-brick O 8 15 BOARD)
              (place-image O
                           (* (+ 1/2 8) BRICK)
                           (* BRICK (- BOARD-HEIGHT (+ 1/2 15)))
                           BOARD))

; brick+scene: Brick Image -> Image
; Adds the brick to the image and creates a new image

(define (brick+scene brick scene)
  (place-brick (overlay (square 20 "outline" "black")
                        (square 20 "solid" (brick-color brick)))
               (brick-x brick)
               (brick-y brick)
               scene))

(check-expect (brick+scene (make-brick 5 10 "orange") BOARD)
              (place-brick (overlay (square 20 "outline" "black")
                                   (square 20 "solid" "orange")) 5 10 BOARD))

(check-expect (brick+scene (make-brick 8 16 "green") BOARD)
              (place-brick (overlay (square 20 "outline" "black")
                                   (square 20 "solid" "green")) 8 16 BOARD))

; tetra+scene: Tetra Image -> Image
; Adds the tetra to the image and creates a new image

(define (tetra+scene tetra scene)
  (bricks->scene (tetra-bricks tetra) scene))

(check-expect (tetra+scene (make-tetra (make-posn 2 1) LOB1) BOARD)
              (bricks->scene (list (make-brick 1 1 "purple")
                                   (make-brick 2 1 "purple")
                                   (make-brick 3 1 "purple")
                                   (make-brick 3 2 "purple")) BOARD))

(check-expect (tetra+scene (make-tetra (make-posn 8 4) LOB2) BOARD)
              (bricks->scene (list (make-brick 8 4 "blue")
                                   (make-brick 8 5 "blue")
                                   (make-brick 8 6 "blue")
                                   (make-brick 8 7 "blue")) BOARD))

; bricks->scene: Bricks Image -> Image
; Adds Bricks (list-of-bricks) to image and creates a new image

(define (bricks->scene bricks scene)
  (cond [(empty? bricks) scene]
        [(cons? bricks)
         (brick+scene (first bricks)
                      (bricks->scene (rest bricks) scene))]))

(check-expect (bricks->scene '() BOARD) BOARD)

(check-expect (bricks->scene (list (make-brick 1 1 "purple")
                                   (make-brick 2 1 "purple")
                                   (make-brick 3 1 "purple")
                                   (make-brick 3 2 "purple")) BOARD)
              (brick+scene (make-brick 1 1 "purple")
                           (brick+scene (make-brick 2 1 "purple")
                                        (brick+scene (make-brick 3 1 "purple")
                                                     (brick+scene (make-brick 3 2 "purple")
                                                                  BOARD)))))

; draw-scene: World -> Image
; Renders an image of the current tetra and blocks in game

(define (draw-scene world)
  (tetra+scene (world-tetra world)
               (bricks->scene (world-pile world) BOARD)))

(check-expect (draw-scene (make-world tetra-ex LOB2))
              (tetra+scene (world-tetra (make-world tetra-ex LOB2))
              (bricks->scene (world-pile (make-world tetra-ex LOB2)) BOARD)))

(check-expect (draw-scene (make-world tetra-ex2 LOB4))
              (tetra+scene (world-tetra (make-world tetra-ex2 LOB4))
              (bricks->scene (world-pile (make-world tetra-ex2 LOB4)) BOARD)))

; random-tetra: Number -> Tetra
; Creates a random tetra based on the number inputted

(define (random-tetra num)
  (cond
    [(= num 0) (make-o 4 18)]
    [(= num 1) (make-i 5 19)]
    [(= num 2) (make-l 4 18)]
    [(= num 3) (make-j 4 18)]
    [(= num 4) (make-t 4 18)]
    [(= num 5) (make-z 5 18)]
    [(= num 6) (make-s 4 18)]))

(check-expect (random-tetra 5)
              (make-tetra (make-posn 5 18) (list (make-brick 5 18 "lightpink")
                                                 (make-brick 6 18 "lightpink")
                                                 (make-brick 5 19 "lightpink")
                                                 (make-brick 4 19 "lightpink"))))
(check-expect (random-tetra 0)
              (make-tetra (make-posn 4 18) (list (make-brick 4 18 "green")
                                                 (make-brick 5 18 "green")
                                                 (make-brick 4 19 "green")
                                                 (make-brick 5 19 "green"))))
                          
;;; Wall Collision

; collide-wall?: Bricks -> Boolean
; Determines whether tetra collides with walls

(define (collide-walls? bricks)
  (cond
    [(empty? bricks) #f]
    [(or (< (brick-x (first bricks)) 0)
         (> (brick-x (first bricks)) 9)
         (collide-walls? (rest bricks))) #t]
    [else #f]))

(check-expect (collide-walls? '()) #f)
(check-expect (collide-walls? LOB2) #f) ; when 0 < x < 9
(check-expect (collide-walls? LOB3) #t) ; when x < 0 or x > 9 

;;; Downwards Collision

; collide?: Tetra Bricks -> Boolean
; Determines whether tetra collides with pile of bricks or bottom

(define (collide? tetra pile)
  (cond
    [(empty? pile)
     (if (bottom-collide? (tetra-bricks tetra)) #t #f)]
    [(or (brick-collide? (tetra-bricks tetra) (first pile))
         (collide? tetra (rest pile))) #t]
    [else #f]))

(check-expect (collide? (make-tetra (make-posn 1 3) '()) LOB1) (bottom-collide? '())) ; empty case
(check-expect (collide? (make-tetra (make-posn 11 5) LOB3) LOB1) #f) ; doesnt collides with brick pile
(check-expect (collide? (make-tetra (make-posn 2 1) LOB1) LOB4) #t) ; collides with bricks 
  
; brick-collide?: Bricks Brick
; Determines whether bricks collide with a brick

(define (brick-collide? bricks brick)
  (cond
    [(empty? bricks) #f]
    [(or (and (= (brick-y brick) (brick-y (first bricks)))
              (= (brick-x brick) (brick-x (first bricks))))
         (brick-collide? (rest bricks) brick)) #t]
    [else #f]))

(check-expect (brick-collide? '() (make-brick 2 1 "brown")) #f) ; empty case
(check-expect (brick-collide? LOB1 (make-brick 2 1 "brown")) #t) ; collides
(check-expect (brick-collide? LOB1 (make-brick 1 8 "brown")) #f) ; no collision

; bottom-collide?: Bricks -> Boolean
; Determines whether bricks collide with bottom

(define (bottom-collide? bricks)
  (cond
    [(empty? bricks) #f]
    [(or (= (brick-y (first bricks)) -1)
         (bottom-collide? (rest bricks))) #t]
    [else #f]))

(check-expect (bottom-collide? '()) #f)
(check-expect (bottom-collide? LOB2) #f)
(check-expect (bottom-collide? (list (make-brick 1 -1 "brown")
                                     (make-brick 1 0 "brown")
                                     (make-brick 1 1 "brown")
                                     (make-brick 1 2 "brown"))) #t)

;;; Tetra Downwards Movement

; bricks-down: Bricks -> Bricks
; Moves all bricks y-values down by one

(define (bricks-down bricks)
  (cond
    [(empty? bricks) bricks]
    [(cons? bricks)
     (cons (make-brick (brick-x (first bricks))
                       (- (brick-y (first bricks)) 1)
                       (brick-color (first bricks)))
           (bricks-down (rest bricks)))]))

(check-expect (bricks-down '()) '())
(check-expect (bricks-down LOB3) (list (make-brick 11 3 "blue")
                                       (make-brick 11 4 "blue")
                                       (make-brick 11 5 "blue")
                                       (make-brick 11 6 "blue")))

;;; Win Condition

; reach-top?: World -> Boolean
; Determines whether pile has reached the top and game should end

(define (reach-top? world)
  (cond
    [(empty? (world-pile world)) #f]
    [(or (= (brick-y (first (world-pile world))) 19)
         (reach-top? (make-world (world-tetra world)
                                 (rest (world-pile world))))) #t]
    [else #f]))


(check-expect (reach-top? emptyworld) #f) ; empty world
(check-expect (reach-top? world1) #f) ; pile has not reached top 
(check-expect (reach-top? toobig-world) #t) ; pile goes over top

;;; On-Tick Function

; update-world: World -> World
; Updates the world based on whether tetra has reached pile

(define (update-world world)
  (cond
    [(collide? (make-tetra (tetra-center (world-tetra world))
                           (bricks-down (tetra-bricks (world-tetra world))))
               (world-pile world))
     (make-world (random-tetra (random 7)) (add-to-pile (tetra-bricks (world-tetra world))
                                                        (world-pile world)))]
    [else (make-world (make-tetra
                       (make-posn (posn-x (tetra-center (world-tetra world)))
                                  (- (posn-y (tetra-center (world-tetra world))) 1))
                       (bricks-down (tetra-bricks (world-tetra world))))
                      (world-pile world))]))

;(check-expect (update-world (collide? (make-tetra (make-posn 11 5) LOB3) LOB1))
              ;(make-world (random-tetra (random 7))
                          ;(add-to-pile (tetra-bricks (world-tetra world))
                                       ;(world-pile world))))


; add-to-pile: Bricks Bricks -> Bricks
; Adds bricks to bricks combines to make one list of bricks

(define (add-to-pile bricks pile)
  (cond
    [(empty? bricks) pile]
    [(cons? bricks)
     (cons (first bricks)
           (add-to-pile (rest bricks) pile))]))

(check-expect (add-to-pile '()  (list (make-brick 1 1 "white")
                                      (make-brick 2 1 "white")
                                      (make-brick 3 1 "white")
                                      (make-brick 4 1 "white")))
              (list (make-brick 1 1 "white")
                    (make-brick 2 1 "white")
                    (make-brick 3 1 "white")
                    (make-brick 4 1 "white"))) 

(check-expect (add-to-pile (list (make-brick 1 2 "green")
                                 (make-brick 2 2 "green")
                                 (make-brick 3 2 "green")
                                 (make-brick 4 2 "green"))
                           (list (make-brick 1 1 "white")
                                 (make-brick 2 1 "white")
                                 (make-brick 3 1 "white")
                                 (make-brick 4 1 "white")))
              (list (make-brick 1 2 "green")
                    (make-brick 2 2 "green")
                    (make-brick 3 2 "green")
                    (make-brick 4 2 "green")
                    (make-brick 1 1 "white")
                    (make-brick 2 1 "white")
                    (make-brick 3 1 "white")
                    (make-brick 4 1 "white")))

;;; Tetra Movement

; move-left: Bricks -> Bricks
; Changes the x value posn by -1 of every brick in list of bricks

(define (move-left bricks)
  (cond
    [(empty? bricks) '()]
    [(cons? bricks)
     (cons (make-brick (- (brick-x (first bricks)) 1)
                       (brick-y (first bricks))
                       (brick-color (first bricks)))
           (move-left (rest bricks)))]))

(check-expect (move-left '()) '())
(check-expect (move-left LOB2) (list (make-brick 7 4 "blue")
                                     (make-brick 7 5 "blue")
                                     (make-brick 7 6 "blue")
                                     (make-brick 7 7 "blue")))

; move-right: Bricks -> Bricks
; Changes the x value posn by +1 of every brick in list of bricks

(define (move-right bricks)
  (cond
    [(empty? bricks) '()]
    [(cons? bricks)
     (cons (make-brick (+ (brick-x (first bricks)) 1)
                       (brick-y (first bricks))
                       (brick-color (first bricks)))
           (move-right (rest bricks)))]))

(check-expect (move-right '()) '())
(check-expect (move-right LOB2) (list (make-brick 9 4 "blue")
                                      (make-brick 9 5 "blue")
                                      (make-brick 9 6 "blue")
                                      (make-brick 9 7 "blue")))


; move-rotate: World -> World
; Handles key-events and calls related functions to move tetra

(define (move-rotate world key-event)
  (cond
    [(key=? key-event "left") (check-left world)]
    [(key=? key-event "right") (check-right world)]
    [(key=? key-event "a") (check-ccw world)]
    [(key=? key-event "s") (check-cw world)]
    [else world]))

; check-left: World -> World
; Handles moving tetra 1 brick to the left

(define (check-left world)
  (cond [(not (or (collide? (make-tetra (tetra-center (world-tetra world))
                                        (move-left (tetra-bricks (world-tetra world))))
                            (world-pile world))
                  (collide-walls? (move-left (tetra-bricks (world-tetra world))))))
         (make-world (make-tetra
                      (make-posn (- (posn-x (tetra-center (world-tetra world))) 1)
                                 (posn-y (tetra-center (world-tetra world))))
                      (move-left (tetra-bricks (world-tetra world))))
                     (world-pile world))]
        [else world]))


(check-expect (check-left world3) ; case for no wall collision
              (make-world (make-tetra
                      (make-posn (- (posn-x (tetra-center (world-tetra world3))) 1)
                                 (posn-y (tetra-center (world-tetra world3))))
                      (move-left (tetra-bricks (world-tetra world3))))
                     (world-pile world3)))


(check-expect (check-left world4) world4) ; case for wall collision 

; check-right: World -> World
; Handles moving tetra 1 brick to the right

(define (check-right world)
  (cond [(not (or (collide? (make-tetra (tetra-center (world-tetra world))
                                        (move-right (tetra-bricks (world-tetra world))))
                            (world-pile world))
                  (collide-walls? (move-right (tetra-bricks (world-tetra world))))))
         (make-world (make-tetra
                      (make-posn (+ (posn-x (tetra-center (world-tetra world))) 1)
                                 (posn-y (tetra-center (world-tetra world))))
                      (move-right (tetra-bricks (world-tetra world))))
                     (world-pile world))]
        [else world]))

(check-expect (check-right world3) ; case for no wall collision 
              (make-world (make-tetra
                           (make-posn (+ (posn-x (tetra-center (world-tetra world3))) 1)
                                      (posn-y (tetra-center (world-tetra world3))))
                           (move-right (tetra-bricks (world-tetra world3))))
                          (world-pile world3)))

(check-expect (check-right (make-world (make-tetra (make-posn 9 10) (list (make-brick 9 6 "purple") ; case for wall collision 
                                                                          (make-brick 9 7 "purple")
                                                                          (make-brick 9 8 "purple")
                                                                          (make-brick 9 9 "purple")))
                           LOB1))
              (make-world (make-tetra (make-posn 9 10) (list (make-brick 9 6 "purple")
                                                             (make-brick 9 7 "purple")
                                                             (make-brick 9 8 "purple")
                                                             (make-brick 9 9 "purple")))
                           LOB1))
              


; check-ccw: World -> World
; Handles rotating tetra counter-clockwise

(define (check-ccw world)
  (cond [(not (or (collide? (make-tetra (tetra-center (world-tetra world))
                                        (bricks-ccw (tetra-bricks (world-tetra world))
                                                    (tetra-center (world-tetra world))))
                            (world-pile world))
                  (collide-walls? (bricks-ccw (tetra-bricks (world-tetra world))
                                              (tetra-center (world-tetra world))))))
         (make-world (make-tetra (tetra-center (world-tetra world))
                                 (bricks-ccw (tetra-bricks (world-tetra world))
                                             (tetra-center (world-tetra world))))
                     (world-pile world))]
        [else world]))

(check-expect (check-ccw world3) ; case for no wall collision 
              (make-world (make-tetra (tetra-center (world-tetra world3))
                                 (bricks-ccw (tetra-bricks (world-tetra world3))
                                             (tetra-center (world-tetra world3))))
                     (world-pile world3)))

(check-expect (check-ccw (make-world (make-tetra (make-posn 1 6) (list (make-brick 1 6 "purple") ; case for rotates into wall
                                                             (make-brick 1 7 "purple")
                                                             (make-brick 1 8 "purple")
                                                             (make-brick 1 9 "purple")))
                           LOB1))
              (make-world (make-tetra (make-posn 1 6) (list (make-brick 1 6 "purple")
                                                             (make-brick 1 7 "purple")
                                                             (make-brick 1 8 "purple")
                                                             (make-brick 1 9 "purple")))
                           LOB1))

               
; check-cw: World -> World
; Handles rotating tetra clockwise

(define (check-cw world)
  (cond [(not (or (collide? (make-tetra (tetra-center (world-tetra world))
                                        (bricks-cw (tetra-bricks (world-tetra world))
                                                   (tetra-center (world-tetra world))))
                            (world-pile world))
                  (collide-walls? (bricks-cw (tetra-bricks (world-tetra world))
                                             (tetra-center (world-tetra world))))))
         (make-world (make-tetra (tetra-center (world-tetra world))
                                 (bricks-cw (tetra-bricks (world-tetra world))
                                            (tetra-center (world-tetra world))))
                     (world-pile world))]
        [else world]))

(check-expect (check-cw world3) ; case for wall collision
              (make-world (make-tetra (tetra-center (world-tetra world3))
                                 (bricks-cw (tetra-bricks (world-tetra world3))
                                            (tetra-center (world-tetra world3))))
                     (world-pile world3)))

(check-expect (check-cw (make-world (make-tetra (make-posn 9 6) (list (make-brick 9 6 "purple") ; case for rotates into wall 
                                                             (make-brick 9 7 "purple")
                                                             (make-brick 9 8 "purple")
                                                             (make-brick 9 9 "purple")))
                           LOB1))
              (make-world (make-tetra (make-posn 9 6) (list (make-brick 9 6 "purple") 
                                                             (make-brick 9 7 "purple")
                                                             (make-brick 9 8 "purple")
                                                             (make-brick 9 9 "purple")))
                           LOB1))
              
              
  
;;; Tetra Rotation

; brick-rotate-ccw: Brick Pt -> Brick
; Rotates a brick counter-clockwise around a given pt

(define (brick-rotate-ccw brick pt)
  (make-brick (+ (posn-x pt)
                 (- (posn-y pt)
                    (brick-y brick)))
              (+ (posn-y pt)
                 (- (brick-x brick)
                    (posn-x pt)))
              (brick-color brick)))

; brick-rotate-cw: Brick Pt -> Brick
; Rotates a brick clockwise around a given pt

(define (brick-rotate-cw brick pt)
  (brick-rotate-ccw (brick-rotate-ccw (brick-rotate-ccw brick pt) pt) pt))

; bricks-ccw: Bricks Pt -> Bricks
; Rotates all bricks counter-clockwise around a given pt

(define (bricks-ccw bricks pt)
  (cond
    [(empty? bricks) '()]
    [(cons? bricks)
     (cons (brick-rotate-ccw (first bricks) pt)
           (bricks-ccw (rest bricks) pt))]))

; bricks-cw: Bricks Pt -> Bricks
; Rotates all bricks clockwise around a given pt

(define (bricks-cw bricks pt)
  (cond
    [(empty? bricks) '()]
    [(cons? bricks)
     (cons (brick-rotate-cw (first bricks) pt)
           (bricks-cw (rest bricks) pt))]))

; draw-end: World -> Image
; Tells user final score after the game ends where
; the final score is the number of bricks in pile

(define (draw-end world)
  (place-image (text (string-append "Score: "
                                    (number->string (length (world-pile world))))
                     24 "black")
               100 200
               BOARD))

; Big-Bang


(big-bang (make-world (random-tetra (random 7)) '())
  [on-tick update-world 0.5]
  [to-draw draw-scene]
  [stop-when reach-top? draw-end]
  [on-key move-rotate])

