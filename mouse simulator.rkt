;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |MOUSE SIMULATOR|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Artificial Artificial Intelligence
; AKA MOUSE SIMULATOR 2000
; Christopher Kei

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;
; CONSTANTS: ;
;;;;;;;;;;;;;;

(define GRID-LENGTH 500)
(define GRID-HEIGHT 500)
(define UI-LENGTH 250)

(define GRASS-STRAND (rectangle 2 8 "solid" "darkolivegreen"))

(define GRASS-POSN-LIST
  (foldr (λ (iteration built-list)
           (cons (make-posn (random GRID-LENGTH) (random GRID-HEIGHT)) built-list))
         '() (build-list 500 identity)))

(define GRASS-BASE (rectangle GRID-LENGTH GRID-HEIGHT "solid" "mediumforestgreen"))
(define OUTDOORS (foldr (λ (p img) (place-image GRASS-STRAND
                                                (posn-x p)
                                                (posn-y p)
                                                img))
                        GRASS-BASE
                        GRASS-POSN-LIST))
(define DIVIDER (rectangle 2 GRID-HEIGHT "solid" "white"))
(define SCORE-KEEPER (rectangle UI-LENGTH GRID-HEIGHT "solid" "lightsalmon"))

(define CHEESE (place-image (ellipse 15 10 "solid" "goldenrod")
                            35
                            8
                            (place-image (ellipse 15 13 "solid" "goldenrod")
                                         15
                                         24
                                         (place-image (ellipse 15 13 "solid" "goldenrod")
                                                      45
                                                      44
                                                      (above (flip-vertical
                                                              (triangle/ass 20 40 60 "solid" "gold"))
                                                             (rectangle 60 1 "solid" "goldenrod")
                                                             (rectangle 60 30 "solid" "gold"))))))

(define MOUSE-BODY-HORIZONTAL (ellipse 30 20 "solid" "lightgray"))
(define MOUSE-INNER-EAR (circle 5 "solid" "pink"))
(define MOUSE-OUTER-EAR (circle 7 "solid" "lightgray"))
(define MOUSE-EAR (overlay MOUSE-INNER-EAR MOUSE-OUTER-EAR))
(define MOUSE-EAR-PAIR (overlay/offset MOUSE-EAR
                                       6 -1 MOUSE-EAR))
(define MOUSE-TAIL-HORIZONTAL (add-curve MOUSE-BODY-HORIZONTAL
                                         -15 10 45 1/2
                                         0 10 45 1/2
                                         (make-pen "gray" 5 "solid" "round" "round")))
(define MOUSE-EYE (circle 2 "solid" "black"))
(define MOUSE-EYE-PAIR (overlay/offset MOUSE-EYE
                                       6 -1 MOUSE-EYE))
(define MOUSE-WHISKERS (above (beside (rectangle 7 1 "solid" "dimgray")
                                      (rectangle 6 1 "solid" "lightgray")
                                      (rectangle 7 1 "solid" "dimgray"))
                              (beside (rectangle 7 1 "solid" "dimgray")
                                      (rectangle 6 1 "solid" "lightgray")
                                      (rectangle 7 1 "solid" "dimgray"))))
(define MOUSE-NOSE (flip-vertical (triangle 4 "solid" "black")))
(define MOUSE-RIGHT (overlay/offset MOUSE-WHISKERS
                                    -15 -7
                                    (overlay/offset MOUSE-EYE-PAIR
                                                    -15 -2
                                                    (overlay/offset MOUSE-EAR-PAIR
                                                                    -13 12
                                                                    MOUSE-TAIL-HORIZONTAL))))
(define MOUSE-LEFT (flip-horizontal MOUSE-RIGHT))
(define MOUSE-WITHOUT-EYES-RIGHT (overlay/offset MOUSE-WHISKERS
                                                 -15 -7
                                                 (overlay/offset MOUSE-EAR-PAIR
                                                                 -13 12
                                                                 MOUSE-TAIL-HORIZONTAL)))
(define MOUSE-WITHOUT-EYES-LEFT (flip-horizontal MOUSE-WITHOUT-EYES-RIGHT))

(define SQUARE-SIZE 10)
(define NUM-SQUARES-LENGTH (/ GRID-LENGTH SQUARE-SIZE))
(define NUM-SQUARES-HEIGHT (/ GRID-HEIGHT SQUARE-SIZE))

(define UP (make-posn 0 -20))
(define DOWN (make-posn 0 20))
(define RIGHT (make-posn 20 0))
(define LEFT (make-posn -20 0))

(define POS-SCALAR 20)

(define INIT-MOUSE-POS (make-posn 60 60))
(define INIT-CHEESE-POS (make-posn 440 440))
(define LIST-OF-Y-FOR-INTERFACE (list 114 126 138 150 162 174 186 198
                                      210 222 234 246 258 270 282 294
                                      306 318 330 342 354 366 378 390
                                      402 414 426 438 450 462))
(define INIT-NUM-MOVES 0)
(define INIT-COUNTER 1)
(define INIT-SCORES-LIST empty)
(define MOUSE-BLIND #false)
(define MOUSE-VISION #true)

(define TRIAL-DIVISOR 5)

(define UI-ADDITIONAL-X 110)
(define SCORES-FIRST-Y 30)
(define SCORES-SECOND-Y 50)
(define SCORES-THIRD-Y 70)
(define SCORES-FOURTH-Y 90)
(define SCORES-ADDITIONAL-X 50)

(define UI-TEXT-SIZE 20)
(define TEXT-COLOR "black")
(define SCORES-TEXT-SIZE 15)
(define FINAL-TEXT-SIZE 30)
(define FINAL-X 375)
(define FINAL-FIRST-Y 200)
(define FINAL-SECOND-Y 230)
(define FINAL-THIRD-Y 260)
(define FINAL-ADDITIONAL-Y 30)

(define INIT-SUM 0)

;;;;;;;;
; DATA ;
;;;;;;;;

; A PosnDir is a (list Posn Posn)
; The first field represents a location
; and the second field represents a beneficial movement that the mouse can make 

(define posndir-0 '())

; A Mouse is a (make-mouse Posn [List-of PosnDir] Boolean)
; It represents the position of the mouse,
; a list of directions that are beneficial for the mouse to travel in
; when on a certain square,
; and whether or not the mouse knows where the cheese is
(define-struct mouse [pos influence found-cheese])

(define mouse-0 (make-mouse INIT-MOUSE-POS posndir-0 MOUSE-BLIND))

; A Cheese is a (make-posn Natural Natural)
; It represents the finish for the mouse

(define cheese-0 INIT-CHEESE-POS)

; A Game is a (make-game Mouse Cheese Natural Natural Natural
;                        [List-of Natural] [List-of Natural] Posn)
; It represents the state of the game
; The game is active if the Mouse has not yet reached the cheese
; The counter activates after the Mouse has found the Cheese to reset the game
; variable explanation:
; - mo = Mouse
; - ch = Cheese
; - num-moves = number of moves made so far
; - counter = which trial the game is on
; - lonm = records the number of moves it has taken for each trial
; - loyp = the y positions at which the scores are listed
; - mo-ini-pos = the Mouse's initial position for restarting purposes
(define-struct game [mo ch num-moves counter lonm loyp mo-init-pos])

(define game-0 (make-game mouse-0
                          cheese-0
                          INIT-NUM-MOVES
                          INIT-COUNTER
                          INIT-SCORES-LIST
                          LIST-OF-Y-FOR-INTERFACE
                          INIT-MOUSE-POS))
(define game-easy (make-game mouse-0
                             (make-posn 120 120)
                             INIT-NUM-MOVES
                             INIT-COUNTER
                             INIT-SCORES-LIST
                             LIST-OF-Y-FOR-INTERFACE
                             INIT-MOUSE-POS))
 
;;;;;;;;;;;;;
; FUNCTIONS ;
;;;;;;;;;;;;;

; transformice : Game -> Game
; Runs the game
(define (transformice g)
  (big-bang g
    (on-tick tick-handler)
    (to-draw draw-handler)
    (stop-when thirty-tries? final)))

; choose-positions : Natural Natural Natural Natural -> Game
; Allows the user to choose where the mouse and cheese spawn
; USER EXPECTATION: ALL NUMBERS MUST BE BETWEEN 0 AND 25
(define (choose-positions mx my cx cy)
  (make-game (make-mouse (make-posn (* POS-SCALAR mx) (* POS-SCALAR my))
                         posndir-0
                         MOUSE-BLIND)
             (make-posn (* POS-SCALAR cx) (* POS-SCALAR cy))
             INIT-NUM-MOVES
             INIT-COUNTER
             INIT-SCORES-LIST
             LIST-OF-Y-FOR-INTERFACE
             (make-posn (* POS-SCALAR mx) (* POS-SCALAR my))))   

; add-posns : Posn Posn -> Posn
; Adds two positions together
(define (add-posns p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))

; same-posn? : Posn Posn -> Boolean
; Determines if two Posns are the same
(define (same-posn? p1 p2)
  (equal? p1 p2))

; tick-handler : Game -> Game
; Changes the position of the mouse,
; adds an element to the mouse's influence if that change is favorable,
; and increases the counter of the mouse if the position
; of the mouse and the cheese are the same
(define (tick-handler g)
  (cond [(not (mouse-found-cheese (game-mo g)))
         (init-find g)]
        [(= 0 (modulo (game-counter g) TRIAL-DIVISOR))
         (make-game (make-mouse (game-mo-init-pos g) (mouse-influence (game-mo g))
                                (mouse-found-cheese (game-mo g)))
                    (game-ch g)
                    INIT-NUM-MOVES
                    (add1 (game-counter g))
                    (cons (game-num-moves g) (game-lonm g))
                    (game-loyp g)
                    (game-mo-init-pos g))]
        [(same-posn? (game-ch g) (mouse-pos (game-mo g)))
         (make-game (game-mo g)
                    (game-ch g)
                    (game-num-moves g)
                    (add1 (game-counter g))
                    (game-lonm g)
                    (game-loyp g)
                    (game-mo-init-pos g))]
        [else
         (make-game (update-mouse (game-mo g) g)
                    (game-ch g)
                    (add1 (game-num-moves g))
                    (game-counter g)
                    (game-lonm g)
                    (game-loyp g)
                    (game-mo-init-pos g))]))

; init-find : Game -> Game
; Has the mouse find the cheese
(define (init-find g)
  (local ((define (choose-random n)
            (local ((define r-num (random n)))
              (cond [(= 0 r-num) UP]
                    [(= 1 r-num) DOWN]
                    [(= 2 r-num) LEFT]
                    [(= 3 r-num) RIGHT]))))
    (if (same-posn? (mouse-pos (game-mo g))
                    (game-ch g))
        (make-game (make-mouse (mouse-pos (game-mo g)) posndir-0 MOUSE-VISION)
                   (game-ch g)
                   (game-num-moves g)
                   (add1 (game-counter g))
                   (game-lonm g)
                   (game-loyp g)
                   (game-mo-init-pos g))
        (make-game (move-mouse/init (game-mo g) (choose-random 4))
                   (game-ch g)
                   (add1 (game-num-moves g))
                   (game-counter g)
                   (game-lonm g)
                   (game-loyp g)
                   (game-mo-init-pos g)))))

; update-mouse : Mouse Game -> Mouse 
; Changes the mouse's position,
; and adds an element to it's influence if that change is favorable
(define (update-mouse m g)
  (local ((define (same-posns-mouse? posdir)
            (same-posn? (first posdir) (mouse-pos m)))
          (define additional
            (map second (filter same-posns-mouse? (mouse-influence m))))
          (define (distance-to-cheese p)
            (+ (abs (- (posn-x (game-ch g)) (posn-x p)))
               (abs (- (posn-y (game-ch g)) (posn-y p)))))
          (define (favorable? p)
            (< (distance-to-cheese (add-posns p (mouse-pos (game-mo g))))
               (distance-to-cheese (mouse-pos (game-mo g)))))
          (define c (update-mouse-picker m g additional)))      
    (if (favorable? c)
        (move-mouse (another-influence m c) c)
        (move-mouse m c))))

; update-mouse-picker : Mouse Game [List-of Posn] -> Posn
; Chooses the number that will determine the next direction of the mose
(define (update-mouse-picker m g lop)
  (local ((define all-choices (append (list UP DOWN LEFT RIGHT) lop))
          (define possibilities (length all-choices)))
    (list-ref all-choices (random possibilities))))

; move-mouse : Mouse Posn -> Mouse
; Changes the mouse's position
(define (move-mouse m p)
  (if (not-out-of-bounds? (add-posns p (mouse-pos m)))
      (make-mouse (add-posns p (mouse-pos m))
                  (mouse-influence m)
                  (mouse-found-cheese m))
      (make-mouse (mouse-pos m)
                  (mouse-influence m)
                  (mouse-found-cheese m))))

; move-mouse/init : Mouse Posn -> Mouse
; Changes the mouse's position
(define (move-mouse/init m p)
  (if (not-out-of-bounds? (add-posns p (mouse-pos m)))
      (make-mouse (add-posns p (mouse-pos m))
                  (mouse-influence m)
                  MOUSE-BLIND)
      (make-mouse (mouse-pos m)
                  (mouse-influence m)
                  MOUSE-BLIND)))

; not-out-of-bounds? Posn -> Boolean
; Makes sure the position will not be out of boundaries
(define (not-out-of-bounds? p)
  (and (> (posn-x p) -1)
       (< (posn-x p) (add1 GRID-LENGTH))
       (> (posn-y p) -1)
       (< (posn-y p) (add1 GRID-HEIGHT))))

; another-influence : Mouse Posn -> Mouse
; Adds another helpful movement to the mouse's knowledge
(define (another-influence m p)
  (make-mouse (mouse-pos m)
              (cons (list (mouse-pos m) p) (mouse-influence m))
              (mouse-found-cheese m)))

; draw-handler : Game -> Image
; Draws the Game
(define (draw-handler g)
  (draw-handler/lonm (game-lonm g) g (game-loyp g)))

; draw-handler/lonm : [List-of Natural] Game [List-of Natural] -> Image
; Draws the Game
(define (draw-handler/lonm lon g loy)
  (local
    ((define (base-img x)
       (place-image
        (text "NUMBER OF MOVES" UI-TEXT-SIZE TEXT-COLOR)
        (+ GRID-LENGTH UI-ADDITIONAL-X)
        SCORES-FIRST-Y
        (place-image
         (text "TO REACH CHEESE  " UI-TEXT-SIZE TEXT-COLOR)
         (+ GRID-LENGTH UI-ADDITIONAL-X)
         SCORES-SECOND-Y
         (place-image
          (text "             (ORDER: NEW -> OLD):          " UI-TEXT-SIZE TEXT-COLOR)
          (+ GRID-LENGTH UI-ADDITIONAL-X)
          SCORES-THIRD-Y
          (place-image
           (text "    -------------------------------" UI-TEXT-SIZE TEXT-COLOR)
           (+ GRID-LENGTH UI-ADDITIONAL-X)
           SCORES-FOURTH-Y
           (place-image x
                        (posn-x (mouse-pos (game-mo g)))
                        (posn-y (mouse-pos (game-mo g)))
                        (place-image CHEESE
                                     (posn-x (game-ch g))
                                     (posn-y (game-ch g))
                                     (beside OUTDOORS DIVIDER SCORE-KEEPER))))))))
     (define reg-img (base-img MOUSE-RIGHT))
     (define irreg-img (base-img MOUSE-LEFT))
     (define no-eyes-right (base-img MOUSE-WITHOUT-EYES-RIGHT))
     (define no-eyes-left (base-img MOUSE-WITHOUT-EYES-LEFT))
     (define (paste x)
       (place-image (text (number->string x)
                          SCORES-TEXT-SIZE TEXT-COLOR)
                    (+ GRID-LENGTH SCORES-ADDITIONAL-X)
                    (first loy)
                    (draw-handler/lonm (rest lon) g (rest loy)))))
    (cond [(not (mouse-found-cheese (game-mo g)))
           (if (>= (posn-x (game-ch g)) (posn-x (mouse-pos (game-mo g))))
               (cond [(empty? lon) no-eyes-right]
                     [else (paste (first lon))])
               (cond [(empty? lon) no-eyes-left]
                     [else (paste (first lon))]))]
          [else
           (if (>= (posn-x (game-ch g)) (posn-x (mouse-pos (game-mo g))))
               (cond [(empty? lon) reg-img]
                     [else (paste (first lon))])
               (cond [(empty? lon) irreg-img]
                     [else (paste (first lon))]))])))

; thirty-tries? : Game -> Boolean
; Determines if the mouse has had 30 different cheese trials
(define (thirty-tries? g)
  (< (* 30 TRIAL-DIVISOR) (game-counter g)))

; final : Game -> Image
; Returns the final image of the game 
(define (final g)
  (results g (draw-handler/lonm  (game-lonm g) (tick-handler
                                                (make-game (game-mo g)
                                                           (game-ch g)
                                                           (game-num-moves g)
                                                           (+ 4 (game-counter g))
                                                           (game-lonm g)
                                                           LIST-OF-Y-FOR-INTERFACE
                                                           (game-mo-init-pos g)))
                                 (game-loyp g))))

; results : Game Image -> Image
; Adds the results of the game to the final screen
(define (results g img)
  (local ((define scores-list (game-lonm g))
          (define avg-1-10
            (inexact->exact
             (round (/ (foldr (λ (n total)
                                (+ (list-ref scores-list n)
                                   total))
                              INIT-SUM (list 0 1 2 3 4 5 6 7 8 9))
                       10))))
          (define avg-11-20
            (inexact->exact
             (round (/ (foldr (λ (n total)
                                (+ (list-ref scores-list n)
                                   total))
                              INIT-SUM (list 10 11 12 13 14 15 16 17 18 19))
                       10))))
          (define avg-21-30
            (inexact->exact
             (round (/ (foldr (λ (n total)
                                (+ (list-ref scores-list n)
                                   total))
                              INIT-SUM (list 20 21 22 23 24 25 26 27 28 29))
                       10))))
          (define avg-list (list avg-1-10 avg-11-20 avg-21-30))
          (define (results-helper l n y)
            (cond [(empty? l) img]
                  [(cons? l)
                   (place-image
                    (text
                     (string-append "Third #" (number->string n) ": "
                                    (number->string (first l))) FINAL-TEXT-SIZE TEXT-COLOR)
                    FINAL-X
                    y
                    (results-helper (rest l) (sub1 n) (+ FINAL-ADDITIONAL-Y y)))])))
    (place-image
     (text "FINAL AVERAGE NUMBER OF MOVES:" FINAL-TEXT-SIZE TEXT-COLOR)
     FINAL-X
     FINAL-FIRST-Y
     (place-image
      (text "-------------------------------------------------------" FINAL-TEXT-SIZE TEXT-COLOR)
      FINAL-X
      FINAL-SECOND-Y
      (results-helper avg-list 3 FINAL-THIRD-Y)))))