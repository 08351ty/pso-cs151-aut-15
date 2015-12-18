;; CMSC 15100, Autumn 2015, University of Chicago
;; Project A
;; prepared by Adam Shaw

;; You are allowed to use this code in subsequent projects. If you choose to
;; use any part of this in your own work, cite it clearly.

#lang typed/racket

(require typed/test-engine/racket-tests)
(require typed/2htdp/image)
(require "../include/uchicago151.rkt")
(require typed/2htdp/universe)

;; === data definitions

(define-struct (Some a)
  ([x : a]))

(define-type (Optional a)
  (U 'none (Some a)))

(define-type Player 
  (U 'black 'white))

(define-struct Loc
  ([row : Integer]   ;; an integer on the interval [0,5]
   [col : Integer])) ;; an integer on the interval [0,5]

(define-type Quadrant
  (U 'NW 'NE 'SW 'SE))

(define-type Direction
  (U 'clockwise 'counterclockwise))

(define-struct Board
  ([NW : (Listof (Optional Player))] ;; these are all lists of length 9
   [NE : (Listof (Optional Player))]
   [SW : (Listof (Optional Player))]
   [SE : (Listof (Optional Player))]))

(define-struct Game
  ([board : Board]
   [next-player : Player]
   [next-action : (U 'place 'twist)]))

(define-type Outcome
  (U Player 'tie))

;; === general purpose utilities

(: isNone? : All (a) (Optional a) -> Boolean)
;; return true iff x is 'none
(define (isNone? x)
  (match x
    ['none #t]
    [_ #f]))

(: isSome? : All (a) (Optional a) -> Boolean)
;; return true iff x is (Some _)
(define (isSome? x)
  (match x
    [(Some _) #t]
    [_ #f]))

(: opt=? : All (a) (a a -> Boolean) -> (Optional a) (Optional a) -> Boolean)
;; makes an equality tester for (Optional a), given an equality test on a
(define (opt=? =?)
  (λ ([opt1 : (Optional a)] [opt2 : (Optional a)])
    (match* (opt1 opt2)
      [('none 'none) #t]
      [((Some x) (Some y)) (=? x y)]
      [(_ _) #f])))

;; === boards and games

(: initial-board Board)
;; empty board
(define initial-board
  (local {(define e (make-list 9 'none))}
    (Board e e e e)))

(: new-game : Game)
;; empty board, white's turn to place a marble
(define new-game
  (Game initial-board 'white 'place))

(: quick-quadrant : Symbol -> (Listof (Optional Player)))
;; makes a quadrant out of a symbol like 'wwb----bb
;; useful for testing
(define (quick-quadrant s)
  (local
    {(: q : Char -> (Optional Player)) 
     (define (q c)
       (match c
         [(or #\w #\W) (Some 'white)]
         [(or #\b #\B) (Some 'black)]
         [_ 'none]))}
    (map q (string->list (symbol->string s)))))

(check-expect (quick-quadrant 'w--b----b)
              (list (Some 'white) 'none 'none
                    (Some 'black) 'none 'none
                    'none 'none (Some 'black)))

(: quick-board : Symbol Symbol Symbol Symbol -> Board)
;; makes a board from four symbols
;; useful for testing
(define (quick-board nw ne sw se)
  (Board (quick-quadrant nw)
         (quick-quadrant ne)
         (quick-quadrant sw)
         (quick-quadrant se)))

(check-expect (quick-board '--------- '--------- '--------- '---------)
              initial-board)

;; === Pentago

(: twist-cw ((Listof (Optional Player)) -> (Listof (Optional Player))))
;; twist a quadrant 90 degrees clockwise
(define (twist-cw q)
  (match q
    [(list s0 s1 s2
           s3 s4 s5
           s6 s7 s8)
     (list s6 s3 s0
           s7 s4 s1
           s8 s5 s2)]
    [_ (error "(Listof (Optional Player)) does not have nine items")]))

(check-expect (twist-cw (quick-quadrant 'www------))
              (quick-quadrant '--w--w--w))
              
(: twist-ccw ((Listof (Optional Player)) -> (Listof (Optional Player))))
;; twist a quadrant 90 degrees counterclockwise
(define (twist-ccw q)
  (match q
    [(list s0 s1 s2
           s3 s4 s5
           s6 s7 s8)
     (list s2 s5 s8
           s1 s4 s7
           s0 s3 s6)]
    [_ (error "Quadrant does not have nine items")]))

(check-expect (twist-ccw (quick-quadrant 'www------))
              (quick-quadrant 'w--w--w--))

(: replace-none ((Listof (Optional Player)) Integer Player -> (Listof (Optional Player))))
;; put a piece at position i in the quadrant iff that position is empty
;; throw an error if something is already there
(define (replace-none q i c)
  (local
    {(: lp : (Listof (Optional Player)) Integer -> (Listof (Optional Player)))
     (define (lp q i)
       (match* (q i)
         [((cons 'none tl) 0) (cons (Some c) tl)]
         [((cons (Some _) _) 0) (error "There's already a marble there.")]
         [((cons hd tl) i) (cons hd (lp tl (sub1 i)))]
         [('() _) (error "empty")]))}
    (lp q i)))

(check-expect (replace-none (quick-quadrant '-wwbbbwww) 0 'black)
              (quick-quadrant 'bwwbbbwww))

(: board-ref : Board Loc -> (Optional Player))
;; return the optional marble at the given location on the board
;; some arithmetic is needed to determine the quadrant and the
;;   position in the quadrant
(define (board-ref b l)
  (match* (b l)
    [((Board NW NE SW SE) (Loc r c))
     (if (and (<= 0 r 5) (<= 0 c 5))
         (local
           {(define q
              (cond
                [(<= 0 r 2) (if (<= 0 c 2) NW NE)]
                [else (if (<= 0 c 2) SW SE)]))}
           (list-ref q (+ (* 3 (remainder r 3)) (remainder c 3))))
         (error (string-append "location out of bounds: row "
                               (number->string r) " col "
                               (number->string c))))]))

(check-expect (board-ref (quick-board 'w--------'b--------
                                      'w--------'b--------)
                         (Loc 0 0))
              (Some 'white))

(check-expect (board-ref (quick-board 'w--------'b--------
                                      'w--------'b--------)
                         (Loc 0 1))
              'none)

(check-expect (board-ref (quick-board 'w--------'b--------
                                      'w--------'b--------)
                         (Loc 0 3))
              (Some 'black))

(check-expect (board-ref (quick-board 'w--------'b--------
                                      'w--------'b--------)
                         (Loc 3 0))
              (Some 'white))

(check-expect (board-ref (quick-board 'w--------'b--------
                                      'w--------'b--------)
                         (Loc 3 3))
              (Some 'black))

(: place-piece (Board Quadrant Integer Player -> Board))
;; put the piece at the given position iff it's empty
;; throws an error, via replace-none, if the place is not open
(define (place-piece bd qid i c)
  (: pquad ((Listof (Optional Player)) -> (Listof (Optional Player))))
  (define (pquad q)
    (replace-none q i c))
  (match bd
    [(Board NW NE SW SE)
     (match qid
       ['NW (Board (pquad NW) NE SW SE)]
       ['NE (Board NW (pquad NE) SW SE)]
       ['SW (Board NW NE (pquad SW) SE)]
       ['SE (Board NW NE SW (pquad SE))])]))

(check-expect (place-piece initial-board 'NW 0 'white)
              (quick-board 'w-------- '--------- '--------- '---------))

(: other : Player -> Player)
;; toggle player
(define (other p)
  (match p ['black 'white] [_ 'black]))

(: place-marble : Game Player Loc -> Game)
;; place marble iff it's the player's turn and the next action is 'place
(define (place-marble g p l)
  (match g
    [(Game _ _ 'twist) (error "Twist is next.")]
    [(Game b q 'place)
     (if (and (<= 0 (Loc-row l) 5) (<= 0 (Loc-col l) 5))
         (if (symbol=? p q)
             (Game (place-piece b (quad-of l) (index-of l) p) p 'twist)
             (error "It's the other player's turn."))
         (error "Off the board."))]))

(check-expect (place-marble new-game 'white (Loc 0 0))
              (Game (quick-board 'w-------- '--------- '--------- '---------)
                    'white
                    'twist))
                                 
(check-error (place-marble new-game 'black (Loc 0 0))
             "It's the other player's turn.")
             
(: quad-of : Loc -> Quadrant)
;; determine which quadrant the location is in
(define (quad-of loc)
  (match loc
    [(Loc r c)
     (if (<= 0 r 2)
         (if (<= 0 c 2) 'NW 'NE)
         (if (<= 0 c 2) 'SW 'SE))]))

(check-expect (quad-of (Loc 1 1)) 'NW)
(check-expect (quad-of (Loc 0 5)) 'NE)
(check-expect (quad-of (Loc 5 0)) 'SW)
(check-expect (quad-of (Loc 4 4)) 'SE)
              
(: index-of : Loc -> Integer)
;; compute the list position of given location within its quadrant
(define (index-of loc)
  (match loc
    [(Loc r c)
     (+ (* 3 (remainder r 3)) (remainder c 3))]))

(check-expect (index-of (Loc 1 0)) 3)
(check-expect (index-of (Loc 2 3)) 6)
(check-expect (index-of (Loc 2 5)) 8)
(check-expect (index-of (Loc 4 1)) 4)
(check-expect (index-of (Loc 5 5)) 8)

(: twist-board : Board Quadrant Direction -> Board)
;; twist a quadrant of the board in the given direction
(define (twist-board b q d)
  (match b
    [(Board NW NE SW SE)
     (local
       {(define twist
          (match d
            ['clockwise twist-cw]
            [_ twist-ccw]))}
       (match q
         ['NW (Board (twist NW) NE SW SE)]
         ['NE (Board NW (twist NE) SW SE)]
         ['SW (Board NW NE (twist SW) SE)]
         ['SE (Board NW NE SW (twist SE))]))]))

(check-expect
 (twist-board (quick-board 'w-------- '--------- '--------- '---------)
              'NW
              'clockwise)
 (quick-board '--w------ '--------- '--------- '---------))

(check-expect
 (twist-board (quick-board 'w-------- '--------- '--------- '---------)
              'NW
              'counterclockwise)
 (quick-board '------w-- '--------- '--------- '---------))

(: twist-quadrant : Game Quadrant Direction -> Game)
;; twist-quadrant and advance to new game state
(define (twist-quadrant g q d)
  (match g
    [(Game b pl 'twist)
     (Game (twist-board b q d) (other pl) 'place)]
    [_ (error "It's not time to twist.")]))

(check-expect
 (twist-quadrant (Game (quick-board 'w-------- '--------- '--------- '---------)
                       'white
                       'twist)
                 'NW
                 'clockwise)
 (Game (quick-board '--w------ '--------- '--------- '---------)
       'black
       'place))

;; === test game end and victory conditions

(: full? : Board -> Boolean)
;; return true iff the board has no empty spaces on it
(define (full? b)
  (match b
    [(Board NW NE SW SE)
     (andmap (inst isSome? Player) (append NW NE SW SE))]))

(check-expect (full? initial-board) #f)

(check-expect (full? (quick-board 'wwwwwwwww 'wwwwwwwww 'bbbbbbbbb 'bbbbbbbbb))
              #t)

(: eastward-row-starting-points : (Listof Loc))
;; these are the 12 spaces from which an eastward row can start
(define eastward-row-starting-points
  (build-list 12 (λ ([i : Integer]) (Loc (quotient i 2) (remainder i 2)))))

(: southward-col-starting-points : (Listof Loc))
;; these are the 12 spaces from which a southward column can start
(define southward-col-starting-points
  (build-list 12 (λ ([i : Integer]) (Loc (remainder i 2) (quotient i 2)))))

(: southeast-diag-starting-points : (Listof Loc))
;; these are the 4 spaces from which a southeast diagonal can start
(define southeast-diag-starting-points
  (list (Loc 0 0) (Loc 0 1) (Loc 1 1) (Loc 1 0)))

(: southwest-diag-starting-points : (Listof Loc))
;; these are the 4 spaces from which a southwest diagonal can start
(define southwest-diag-starting-points
  (list (Loc 0 4) (Loc 0 5) (Loc 1 4) (Loc 1 5)))

(: make-mover : (Integer -> Integer) (Integer -> Integer) -> (Loc -> Loc))
;; make a Loc -> Loc function out of a row modifier and a col modifier
(define (make-mover do-to-row do-to-col)
  (λ ([loc : Loc])
    (match loc
      [(Loc r c) (Loc (do-to-row r) (do-to-col c))])))

(check-expect ((make-mover identity identity) (Loc 0 0)) (Loc 0 0))
(check-expect ((make-mover add1 identity) (Loc 0 0)) (Loc 1 0))
(check-expect ((make-mover identity add1) (Loc 0 0)) (Loc 0 1))
(check-expect ((make-mover add1 add1) (Loc 0 0)) (Loc 1 1))

(: follow-five : Board Player Loc (Loc -> Loc) -> Boolean)
;; given the board, a player, a starting location and
;; a way to move from location to location, see if there
;; are five adjacent locations containing that player
(define (follow-five b p start next)
  (local
    {(: lp : Loc Integer -> Boolean)
     ;; given location and number of squares remaining to check,
     ;; return true if all squares contain player p
     (define (lp current-loc locs-remaining)
       (if (zero? locs-remaining)
           #t
           (and ((opt=? symbol=?) (Some p) (board-ref b current-loc))
                (lp (next current-loc) (sub1 locs-remaining)))))}
    (lp start 5)))

(check-expect
 (follow-five (quick-board 'www------ 'ww------- '--------- '---------)
              'white
              (Loc 0 0)
              (make-mover identity add1))
 #t)

(check-expect
 (follow-five (quick-board 'w--w--w-- '--------- 'w--w----- '---------)
              'white
              (Loc 0 0)
              (make-mover identity add1))
 #f)

(check-expect
 (follow-five (quick-board 'w--w--w-- '--------- 'w--w----- '---------)
              'white
              (Loc 0 0)
              (make-mover add1 identity))
 #t)

(check-expect
 (follow-five (quick-board 'w---w---w '--------- '--------- 'w---w----)
              'white
              (Loc 0 0)
              (make-mover add1 add1))
 #t)

(: row? : Board Player -> Boolean)
;; does the given player have five in a row anywhere on the board?
(define (row? b p)
  (ormap (λ ([start : Loc]) (follow-five b p start (make-mover identity add1)))
         eastward-row-starting-points))

(check-expect
 (row? (quick-board 'www------ 'ww------- '--------- '---------) 'white)
 #t)

(check-expect
 (row? (quick-board 'www------ 'ww------- '--------- '---------) 'black)
 #f)

(check-expect
 (row? (quick-board 'wwwww---- '--------- '------bbb '------bb-) 'black)
 #t)

(: col? : Board Player -> Boolean)
;; does the given player have five in a column anywhere on the board?
(define (col? b p)
  (ormap (λ ([start : Loc]) (follow-five b p start (make-mover add1 identity)))
         southward-col-starting-points))

(check-expect
 (col? (quick-board 'www------ 'ww------- '--------- '---------) 'white)
 #f)

(check-expect
 (col? (quick-board 'w--w--w-- '--------- 'w--w----- '---------) 'white)
 #t)

(: diag? : Board Player -> Boolean)
;; does the given player have five in a diagonal anywhere on the board?
(define (diag? b p)
  (or
   (ormap (λ ([start : Loc]) (follow-five b p start (make-mover add1 add1)))
          southeast-diag-starting-points)
   (ormap (λ ([start : Loc]) (follow-five b p start (make-mover add1 sub1)))
          southwest-diag-starting-points)))

(check-expect
 (diag? (quick-board 'w---w---w '--------- '--------- 'w---w----) 'white)
 #t)

(check-expect
 (diag? (quick-board 'w---w---w '--b-b-b-- '--b-b---- 'w---w----) 'black)
 #t)
   
(: has-five? : Game Player -> Boolean)
;; did the player get five in a row?
(define (has-five? g p)
  (match g
    [(Game b _ _)
     (or (row? b p) (col? b p) (diag? b p))]))

(check-expect (has-five? new-game 'white) #f)
(check-expect (has-five? new-game 'black) #f)
(check-expect
 (has-five? (Game
             (quick-board 'w---w---w '--b-b-b-- '--b-b---- 'w---w----)
             'white
             'twist)
            'white)
 #t)
(check-expect
 (has-five? (Game
             (quick-board 'w---w---w '--b-b-b-- '--b-b---- 'w---w----)
             'white
             'twist)
            'black)
 #t)

(: game-over? : Game -> Boolean)
;; is the game over?
(define (game-over? g)
  (or (has-five? g 'white)
      (has-five? g 'black)
      (full? (Game-board g))))

(: outcome : Game -> Outcome)
(define (outcome g)
  (match* ((has-five? g 'white) (has-five? g 'black))
    [(#t #t) 'tie]
    [(#t _) 'white]
    [(_ #t) 'black]
    [(_ _) 'tie]))

;; === visualization

(: square-image : (Optional Player) -> Image)
(define (square-image p)
  (frame
   (match p
     ['none (square 30 'solid 'gainsboro)]
     [(Some p) (overlay (circle 10 'solid p) (square 30 'solid 'gainsboro))])))

(: quadrant-image : (Listof (Optional Player)) -> Image)
(define (quadrant-image q)
  (match (map square-image q)
    [(list a b c d e f g h i)
     (above (beside a b c)
            (beside d e f)
            (beside g h i))]))

(: sbr : Integer Integer -> Image)
;; sbr = "solid black rectangle"
(define (sbr width height)
  (rectangle (max 0 width) (max 0 height) 'solid 'black))

(: scale-to-width : Image Integer -> Image)
;; scale image to desired width
(define (scale-to-width img w)
  (if (<= w 0)
      (error "Width must be positive.")
      (local
        {(define scalar
           (/ w (image-width img)))}
        (if (<= scalar 0)
            (error "bug") ;; this is just to "outwit" the typechecker
            (scale scalar img)))))

(: board-image : Board Integer -> Image)
(define (board-image b desired-width)
  (match b
    [(Board NW NE SW SE)
     (match (map quadrant-image (list NW NE SW SE))
       [(list i1 i2 i3 i4)
        (local
          {(define δ 4)
           (define vspace (sbr δ (image-height i1)))
           (define hspace (sbr (+ δ (* 2 (image-width i1))) δ))
           (define result
             (above (beside i1 vspace i2)
                    hspace
                    (beside i3 vspace i4)))}
          (scale-to-width result desired-width))])]))

;;change to byte for text
(: int->byte : Integer -> Positive-Byte)
(define (int->byte n)
  (cond
    [(<= n 1) 1]
    [(>= n 255) 255]
    [else n]))
(check-expect (int->byte 256) 255)
(check-expect (int->byte 0) 1)

(: game-image : Game Integer -> Image)
(define (game-image g desired-width)
  (match g
    [(Game b pl act) (if (> desired-width 0) (above (board-image (Game-board g) desired-width)
                            (overlay (above (text (string-append (symbol->string pl) "'s turn") (int->byte (quotient desired-width 20)) "black")
                         (rectangle desired-width (quotient desired-width 35) "solid" "gainsboro")
                         (text (symbol->string act) (int->byte (quotient desired-width 20)) "black")
                         (rectangle desired-width (quotient desired-width 70) "solid" "gainsboro")
                         (text "click on square to place ; click on quadrant to select quadrant to twist" (int->byte (quotient desired-width 50)) "black")
                         (rectangle desired-width (quotient desired-width 70) "solid" "gainsboro")
                         (text "left arrow key to turn selected quadranted counterclockwise" (int->byte (quotient desired-width 50)) "black")
                         (rectangle desired-width (quotient desired-width 70) "solid" "gainsboro")
                         (text "right arrow key to turn selected quadrant clockwise" (int->byte (quotient desired-width 50)) "black"))
                  (overlay (rectangle desired-width (quotient desired-width 4) "outline" "black") (rectangle desired-width (quotient desired-width 4) "solid" "gainsboro"))))
                         empty-image)]))

(define-struct Move
  ([loc : Loc]
   [q   : Quadrant]
   [dir : Direction]))

(define-type Human
  (U String Symbol))

(define-struct Bot
  ([name : (U String Symbol)]
   [mind : (Game (Listof Loc) -> (Optional Move))]))

(define-struct World
  ([game : Game]
   [player1 : (U Human Bot)]
   [player2 : (U Human Bot)]
   [bots-turn? : Boolean]))

;;determines whether player is a bot
(: is-bot? : (U Human Bot) -> Boolean)
(define (is-bot? player)
  (cond
    [(or (symbol? player) (string? player)) #f]
    [else #t]))
(check-expect (is-bot? "adam") #f)
(check-expect (is-bot? (Bot "droid" first-available)) #t)

;;initializes world
(: pentago : (U Human Bot) (U Human Bot) -> World)
(define (pentago p1 p2)
  (local
    {(define empty-quad (make-list 9 'none))
     (define empty-board (Board empty-quad empty-quad empty-quad empty-quad))
     (define empty-game (Game empty-board 'white 'place))
     (define world0
       (cond
         [(or (symbol? p1) (string? p1)) (World empty-game p1 p2 #f)]
         [else (keypress
                (handle-mouse (World empty-game p1 p2 #t)
                              10
                              10
                              "button-down")
                "right")]))}
    (big-bang world0 : World
              [to-draw draw]
              [on-mouse handle-mouse]
              [on-key keypress]
              ;;[on-tick handle-tick]
              [stop-when end-game])))

;;draws environment
(: draw : World -> Image)
(define (draw w)
  (if (end-game w)
      (overlay
       (text (match (outcome (World-game w))
               ['black "black wins!"]
               ['white "white wins!"]
               ['tie "it's a tie!"]) 40 "black")
       (rectangle 300 200 "solid" "maroon")
       (overlay/align "left" "top"
                      (match selected-quadrant
                        ['NW (square 250 "outline" "cyan")]
                        ['NE (beside
                              (square 250 "outline" (color 128 128 128 0))
                              (square 250 "outline" "cyan"))]
                        ['SW (above
                              (square 250 "outline" (color 128 128 128 0))
                              (square 250 "outline" "cyan"))]
                        ['SE (above
                              (rectangle 500 250 "outline" (color 128 128 128 0))
                              (beside
                               (square 250 "outline" (color 128 128 128 0))
                               (square 250 "outline" "cyan")))])             
                      (game-image (World-game w) 500)))
      (overlay/align "left" "top" (match selected-quadrant
                                    ['NW (square 250 "outline" "cyan")]
                                    ['NE (beside (square 250 "outline" (color 128 128 128 0))
                                                 (square 250 "outline" "cyan"))]
                                    ['SW (above (square 250 "outline" (color 128 128 128 0))
                                                (square 250 "outline" "cyan"))]
                                    ['SE (above (rectangle 500 250 "outline" (color 128 128 128 0))
                                                (beside (square 250 "outline" (color 128 128 128 0))
                                                        (square 250 "outline" "cyan")))])
                     
                     (game-image (World-game w) 500))))

;;Handles mouse click to place marble
(: handle-mouse : World Integer Integer Mouse-Event -> World)
(define (handle-mouse w x y e)
  (match e
    ["button-down" (if (symbol=? (Game-next-action (World-game w)) 'place)
                       (cond
                         [(and (< x 80) (< y 80))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 0 0))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 80 x 160) (< y 80))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 0 1))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 160 x 240) (< y 80))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 0 2))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 260 x 340) (< y 80))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 0 3))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 340 x 420) (< y 80))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 0 4))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 420 x 500) (< y 80))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 0 5))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< x 80) (< 80 y 160))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 1 0))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 80 x 160) (< 80 y 160) )
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 1 1))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 160 x 240) (< 80 y 160))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 1 2))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 260 x 340) (< 80 y 160))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 1 3))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 340 x 420) (< 80 y 160))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 1 4))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 420 x 500) (< 80 y 160))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 1 5))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< x 80) (< 160 y 240))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 2 0))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 80 x 160) (< 160 y 240))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 2 1))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 160 x 240) (< 160 y 240))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 2 2))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 260 x 340) (< 160 y 240))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 2 3))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 340 x 420) (< 160 y 240))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 2 4))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 420 x 500) (< 160 y 240))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 2 5))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< x 80) (< 260 y 340))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 3 0))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 80 x 160) (< 260 y 340))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 3 1))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 160 x 240) (< 260 y 340))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 3 2))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 260 x 340) (< 260 y 340))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 3 3))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 340 x 420) (< 260 y 340))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 3 4))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 420 x 500) (< 260 y 340))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 3 5))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< x 80) (< 340 y 420))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 4 0))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 80 x 160) (< 340 y 420))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 4 1))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 160 x 240) (< 340 y 420))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 4 2))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 260 x 340) (< 340 y 420))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 4 3))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 340 x 420) (< 340 y 420))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 4 4))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 420 x 500) (< 340 y 420))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 4 5))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< x 80) (< 420 y 500))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 5 0))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 80 x 160) (< 420 y 500))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 5 1))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 160 x 240) (< 420 y 500))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 5 2))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 260 x 340) (< 420 y 500))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 5 3))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 340 x 420) (< 420 y 500))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 5 4))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [(and (< 420 x 500) (< 420 y 500))
                          (World (place-marble (World-game w)
                                               (Game-next-player (World-game w))
                                               (Loc 5 5))
                                 (World-player1 w)
                                 (World-player2 w)
                                 (World-bots-turn? w))]
                         [else w])
                       (cond
                         [(and (< x 250) (< y 250)) (set! selected-quadrant 'NW) w]
                         [(and (< 250 x 500) (< y 250)) (set! selected-quadrant 'NE) w]
                         [(and (< x 250) (< 250 y 500)) (set! selected-quadrant 'SW) w]
                         [(and (< 250 x 500) (< 250 y 500)) (set! selected-quadrant 'SE) w]
                         [else w]))]
    [_ w]))
(check-expect (handle-mouse (World (Game (Board (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none))
                                        'black
                                        'place)
                            "adam"
                            "jack"
                            #f)
                            450
                            450
                            "button-down")
              (World (Game (Board (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none (Some 'black)))
                                        'black
                                        'twist)
                            "adam"
                            "jack"
                            #f))

;;selected quadrant to twist
(define selected-quadrant 'NW)

;;determines whether opponent is human or bot
(: player-color : World -> (U Human Bot))
(define (player-color w)
  (match (Game-next-player (World-game w))
    ['white (World-player2 w)]
    ['black (World-player1 w)]
    [else (error "neither black nor white")]))
(check-expect (player-color (World (Game (Board (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none (Some 'black)))
                                        'black
                                        'twist)
                            "adam"
                            "jack"
                            #f))
              "adam")

;;determines whether self is human or bot
(: self-color : World -> (U Human Bot))
(define (self-color w)
  (match (Game-next-player (World-game w))
    ['white (World-player1 w)]
    ['black (World-player2 w)]
    [else (error "neither black nor white")]))
(check-expect (self-color (World (Game (Board (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none (Some 'black)))
                                        'black
                                        'twist)
                            "adam"
                            "jack"
                            #f))
              "jack")

;;rotates selected quadrant clockwise or counterclockwise
(: keypress : World String -> World)
(define (keypress w s)
  (match (Game-next-action (World-game w))
    ['twist (match s
              ["left" (match selected-quadrant
                        ['NW (if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'NW
                                                                     'counterclockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))
                                (World (twist-quadrant (World-game w)
                                                             'NW
                                                             'counterclockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w))))]
                        ['NE (if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'NE
                                                                     'counterclockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))
                                (World (twist-quadrant (World-game w)
                                                             'NE
                                                             'counterclockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w))))]
                        ['SW (if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'SW
                                                                     'counterclockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))
                                (World (twist-quadrant (World-game w)
                                                             'SW
                                                             'counterclockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w))))]
                        ['SE (if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'SE
                                                                     'counterclockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))
                                (World (twist-quadrant (World-game w)
                                                             'SE
                                                             'counterclockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w))))])]
              ["right" (match selected-quadrant
                         ['NW (cond
                                [(and (is-bot? (player-color w)) (is-bot? (self-color w)))
                                 (continuous-bot-move (World (twist-quadrant (World-game w)
                                                                     'NW
                                                                     'clockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))]
                                [(is-bot? (player-color w))
                          ;;(if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'NW
                                                                     'clockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))]
                                [else (World (twist-quadrant (World-game w)
                                                             'NW
                                                             'clockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w)))])]
                        ['NE (if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'NE
                                                                     'clockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))
                                (World (twist-quadrant (World-game w)
                                                             'NE
                                                             'clockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w))))]
                         ['SW (if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'SW
                                                                     'clockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))
                                (World (twist-quadrant (World-game w)
                                                             'SW
                                                             'clockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w))))]
                         ['SE (if (is-bot? (player-color w))
                                (bot-move (World (twist-quadrant (World-game w)
                                                                     'SE
                                                                     'clockwise)
                                                     (World-player1 w)
                                                     (World-player2 w)
                                                     (is-bot? (player-color w))))
                                (World (twist-quadrant (World-game w)
                                                             'SE
                                                             'clockwise)
                                             (World-player1 w)
                                             (World-player2 w)
                                             (is-bot? (player-color w))))])]
              [_ w])]
    [_ w]))
(check-expect (keypress (World (Game (Board (list (Some 'black) 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none (Some 'black)))
                                        'black
                                        'twist)
                            "adam"
                            "jack"
                            #f)
                        "right")
              (World (Game (Board (list 'none 'none (Some 'black)
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none 'none)
                                               (list 'none 'none 'none
                                                     'none 'none 'none
                                                     'none 'none (Some 'black)))
                                        'white
                                        'place)
                            "adam"
                            "jack"
                            #f))
              
;;if both players are bots, recursively apply bot moves
(: continuous-bot-move : World -> World)
(define (continuous-bot-move w)
  (if (or (full? (Game-board (World-game w))) (end-game w))
      w
      (continuous-bot-move (bot-move w))))

(: get-bot : (U Human Bot) -> Bot)
(define (get-bot b)
  (if (or (symbol? b) (string? b))
      (error "not a bot")
      b))

(: get-move : (U 'none (Some Move)) -> Move)
(define (get-move m)
  (match m
    [(Some x) x]
    [_ (error "no move")]))
;;applies bot move
;;1.) place marble at first available square
;;2.) twist NW quadrant clockwise
(: bot-move : World -> World)
(define (bot-move w)
;  (cond
;    [(or (symbol? (self-color w)) (string? (self-color w))) w]
;    [else 
;  ;;(apply-twist (apply-move (World-game w) loc-list) w))
;     (apply-twist (apply-move (Move-loc (get-move ((Bot-mind (get-bot (self-color w))) (World-game w) loc-list))) w)
;                  (Move-q (get-move ((Bot-mind (get-bot (self-color w))) (World-game w) loc-list)))
;                  (Move-dir (get-move ((Bot-mind (get-bot (self-color w))) (World-game w) loc-list))))]))
  (apply-move ((Bot-mind (get-bot (self-color w))) (World-game w) loc-list) w))



;;determines if game has ended
(: end-game : World -> Boolean)
(define (end-game w)
  (game-over? (World-game w)))
(check-expect (end-game 
(World
 (Game
  (Board
   (list
    (Some 'black)
    (Some 'black)
    (Some 'white)
    (Some 'white)
    (Some 'white)
    (Some 'white)
    (Some 'white)
    (Some 'black)
    (Some 'black))
   (list
    (Some 'black)
    (Some 'white)
    (Some 'black)
    (Some 'black)
    (Some 'white)
    (Some 'black)
    (Some 'white)
    (Some 'black)
    (Some 'white))
   (list
    (Some 'black)
    (Some 'white)
    (Some 'black)
    (Some 'black)
    (Some 'white)
    (Some 'black)
    (Some 'black)
    (Some 'white)
    (Some 'black))
   (list
    (Some 'white)
    (Some 'black)
    (Some 'white)
    (Some 'white)
    (Some 'black)
    (Some 'white)
    (Some 'white)
    (Some 'black)
    (Some 'white)))
  'black
  'place)
 "dom"
 "mazetti"
 #t)) #t)

;;list of locations
(: loc-list : (Listof Loc))
(define loc-list
  (list (Loc 0 0) (Loc 0 1) (Loc 0 2) (Loc 0 3) (Loc 0 4) (Loc 0 5)
        (Loc 1 0) (Loc 1 1) (Loc 1 2) (Loc 1 3) (Loc 1 4) (Loc 1 5)
        (Loc 2 0) (Loc 2 1) (Loc 2 2) (Loc 2 3) (Loc 2 4) (Loc 2 5)
        (Loc 3 0) (Loc 3 1) (Loc 3 2) (Loc 3 3) (Loc 3 4) (Loc 3 5)
        (Loc 4 0) (Loc 4 1) (Loc 4 2) (Loc 4 3) (Loc 4 4) (Loc 4 5)
        (Loc 5 0) (Loc 5 1) (Loc 5 2) (Loc 5 3) (Loc 5 4) (Loc 5 5)))

;;determines if the marble that is placed is okay
(: place-marble-ok? : Game Loc -> Boolean)
(define (place-marble-ok? g l)
  (match (board-ref (Game-board g) l)
    ['none #t]
    [_ #f]))
(check-expect (place-marble-ok? (Game
             (quick-board 'w---w---w '--b-b-b-- '--b-b---- 'w---w----)
             'white
             'twist)
                                (Loc 0 1))
              #t)
(check-expect (place-marble-ok? (Game
             (quick-board 'w---w---w '--b-b-b-- '--b-b---- 'w---w----)
             'white
             'twist)
                                (Loc 0 0))
              #f)

;;finds first available marble to place and creates move
;;which has Loc of available placement, 'NW quadrant and 'clockwise
(: first-available : Game (Listof Loc) -> (Optional Move))
(define (first-available g list)
  (match list
    ['() (error "empty list")]
    [(cons hd '()) (if (place-marble-ok? g hd)
                       (Some (Move hd 'NW 'clockwise))
                       'none)]
    [(cons hd tl) (if (place-marble-ok? g hd)
                      (Some (Move hd 'NW 'clockwise))
                      (first-available g tl))]))
(check-expect (first-available (Game
             (quick-board 'w---w---w '--b-b-b-- '--b-b---- 'w---w----)
             'white
             'twist)
                                loc-list)
              (Some (Move (Loc 0 1) 'NW 'clockwise)))


;;place marble at earliest possible Loc
(: apply-move : (Optional Move) World -> World)
(define (apply-move m w)
  (match m
    ['none w]
    [(Some (Move l q dir)) (World (twist-quadrant (place-marble (World-game w)
                                                                (Game-next-player (World-game w))
                                                                l)
                                                  q
                                                  dir)
                                  (World-player1 w)
                                  (World-player2 w)
                                  (World-bots-turn? w))]))

;;twists NW quadrant clockwise
(: apply-twist : World Quadrant Direction -> World)
(define (apply-twist w q d)
  (World (twist-quadrant (World-game w)
                         q
                         d)
         (World-player1 w)
         (World-player2 w)
         (World-bots-turn? w)))

(define-type Heuristic
  (Player Board -> Integer))

(: opponent : Player -> Player)
;;finds opponent
(define (opponent p)
  (match p
    ['white 'black]
    ['black 'white]))

(: longest-right : Player Board Loc Integer -> Integer)
;;finds the longest run goin horizontal for given location
(define (longest-right p b l combo)
  (if (= (Loc-col l) 5)
      combo
      (match (board-ref b l)
        ['none (longest-right p b (Loc (Loc-row l) (+ (Loc-col l) 1)) combo)]
        [(Some 'white) (if (symbol=? p 'white)
                           (longest-right p b (Loc (Loc-row l) (+ (Loc-col l) 1)) (+ combo 1))
                           0)]
        [(Some 'black) (if (symbol=? p 'black)
                           (longest-right p b (Loc (Loc-row l) (+ (Loc-col l) 1)) (+ combo 1))
                           0)])))
(check-expect (longest-right 'white
                             (quick-board 'www-w---w 'w------bb '------bbb 'w---w----)
                             (Loc 0 0)
                             0) 4)
(check-expect (longest-right 'white
                             (quick-board 'www-w---w 'b------bb '------bbb 'w---w----)
                             (Loc 0 0)
                             0) 0)
(check-expect (longest-right 'black
                             (quick-board 'www-w---w 'b------bb '------bbb 'w---w----)
                             (Loc 5 0)
                             0) 3)

(: max-right : Player Board -> Integer)
;;finds the largest run for all horizontal runs
(define (max-right p b)
  (max (longest-right p b (Loc 0 0) 0)
       (longest-right p b (Loc 1 0) 0)
       (longest-right p b (Loc 2 0) 0)
       (longest-right p b (Loc 3 0) 0)
       (longest-right p b (Loc 4 0) 0)
       (longest-right p b (Loc 5 0) 0)))
(check-expect (max-right 'white
                             (quick-board 'w-w-w---w '-w-----bb '------bbb 'w---w----)) 3)
(check-expect (max-right 'white
                             (quick-board 'w-w-w---w 'w------bb 'www---bbb 'w---w----)) 4)

(: sum-right : Player Board -> Integer)
;;sums the square of all horizontal runs
(define (sum-right p b)
  (+ (expt (longest-right p b (Loc 0 0) 0) 2)
     (expt (longest-right p b (Loc 1 0) 0) 2)
     (expt (longest-right p b (Loc 2 0) 0) 2)
     (expt (longest-right p b (Loc 3 0) 0) 2)
     (expt (longest-right p b (Loc 4 0) 0) 2)
     (expt (longest-right p b (Loc 5 0) 0) 2)))
(check-expect (sum-right 'white
                         (quick-board 'w-w-w---w '-w-----bb '------bbb 'w---w----)) 12)

(: longest-down : Player Board Loc Integer -> Integer)
;;finds the longest run going vertical for given location
(define (longest-down p b l combo)
  (if (= (Loc-row l) 5)
      combo
      (match (board-ref b l)
        ['none (longest-down p b (Loc (+ (Loc-row l) 1) (Loc-col l)) combo)]
        [(Some 'white) (if (symbol=? p 'white)
                           (longest-down p b (Loc (+ (Loc-row l) 1) (Loc-col l)) (+ combo 1))
                           0)]
        [(Some 'black) (if (symbol=? p 'black)
                           (longest-down p b (Loc (+ (Loc-row l) 1) (Loc-col l)) (+ combo 1))
                           0)])))
(check-expect (longest-down 'white
                             (quick-board 'www-w---w 'w------bb '------bbb 'w---w----)
                             (Loc 0 0)
                             0) 1)

(: max-down : Player Board -> Integer)
;;finds the largest run for all vertical runs
(define (max-down p b)
  (max (longest-down p b (Loc 0 0) 0)
       (longest-down p b (Loc 0 1) 0)
       (longest-down p b (Loc 0 2) 0)
       (longest-down p b (Loc 0 3) 0)
       (longest-down p b (Loc 0 4) 0)
       (longest-down p b (Loc 0 5) 0)))
(check-expect (max-down 'white
                             (quick-board 'w-w-w---w '-w-----bb '------bbb 'w---w----)) 2)
(check-expect (max-down 'white
                             (quick-board 'w-w-w---w 'w------bb 'www---bbb 'w---w----)) 3)

(: sum-down : Player Board -> Integer)
;;finds the sum of the square of all vertical runs
(define (sum-down p b)
  (+ (expt (longest-down p b (Loc 0 0) 0) 2)
     (expt (longest-down p b (Loc 0 1) 0) 2)
     (expt (longest-down p b (Loc 0 2) 0) 2)
     (expt (longest-down p b (Loc 0 3) 0) 2)
     (expt (longest-down p b (Loc 0 4) 0) 2)
     (expt (longest-down p b (Loc 0 5) 0) 2)))
(check-expect (sum-down 'white
                             (quick-board 'w-w-w---w '-w-----bb '------bbb 'w---w----)) 7)
  
(: longest-diag1 : Player Board Loc Integer -> Integer)
;;finds the longest diagonal from NW to SE
(define (longest-diag1 p b l combo)
  (if (or (= (Loc-row l) 5) (= (Loc-col l) 5))
      combo
      (match (board-ref b l)
        ['none (longest-diag1 p b (Loc (+ (Loc-row l) 1) (+ (Loc-col l) 1)) combo)]
        [(Some 'white) (if (symbol=? p 'white)
                           (longest-diag1 p b (Loc (+ (Loc-row l) 1) (+ (Loc-col l) 1)) (+ combo 1))
                           0)]
        [(Some 'black) (if (symbol=? p 'black)
                           (longest-diag1 p b (Loc (+ (Loc-row l) 1) (+ (Loc-col l) 1)) (+ combo 1))
                           0)])))
(check-expect (longest-diag1 'white
                             (quick-board '-ww-w---w 'w------bb '------bbb 'w---w----)
                             (Loc 0 0)
                             0) 4)

(: max-diag1 : Player Board -> Integer)
;;finds the max diagonal run from NW to SE
(define (max-diag1 p b)
  (max (longest-diag1 p b (Loc 0 1) 0)
       (longest-diag1 p b (Loc 0 0) 0)
       (longest-diag1 p b (Loc 1 0) 0)))
(check-expect (max-diag1 'white
                             (quick-board 'w-w-w---w '-w-----bb '------bbb 'w---w----)) 5)
(check-expect (max-diag1 'white
                             (quick-board 'w-w-w---w 'w------bb 'www---bbb 'w--------)) 4)

(: sum-diag1 : Player Board -> Integer)
;;finds the sum of square of all diagonal runs from NW to SE
(define (sum-diag1 p b)
  (+ (expt (longest-diag1 p b (Loc 0 1) 0) 2)
     (expt (longest-diag1 p b (Loc 0 0) 0) 2)
     (expt (longest-diag1 p b (Loc 1 0) 0) 2)))
(check-expect (sum-diag1 'white
                         (quick-board 'w-w-w---w 'w------bb 'www---bbb 'w--------)) 17)

(: longest-diag2 : Player Board Loc Integer -> Integer)
;;finds the longest diagonal from NE to SW
(define (longest-diag2 p b l combo)
  (if (or (= (Loc-row l) 5) (= (Loc-col l) 0))
      combo
      (match (board-ref b l)
        ['none (longest-diag2 p b (Loc (+ (Loc-row l) 1) (- (Loc-col l) 1)) combo)]
        [(Some 'white) (if (symbol=? p 'white)
                           (longest-diag2 p b (Loc (+ (Loc-row l) 1) (- (Loc-col l) 1)) (+ combo 1))
                           0)]
        [(Some 'black) (if (symbol=? p 'black)
                           (longest-diag2 p b (Loc (+ (Loc-row l) 1) (- (Loc-col l) 1)) (+ combo 1))
                           0)])))
(check-expect (longest-diag2 'white
                             (quick-board '-ww-w---w 'w---w-wbb '----w-bbb 'w---w----)
                             (Loc 0 5)
                             0) 3)

(: max-diag2 : Player Board -> Integer)
;;finds the maximum diagonal run from NE to SW
(define (max-diag2 p b)
  (max (longest-diag1 p b (Loc 0 5) 0)
       (longest-diag1 p b (Loc 0 4) 0)
       (longest-diag1 p b (Loc 1 5) 0)))

(: sum-diag2 : Player Board -> Integer)
;;finds the sum of the square of all diagonal runs from NE to SW
(define (sum-diag2 p b)
  (+ (expt (longest-diag1 p b (Loc 0 5) 0) 2)
     (expt (longest-diag1 p b (Loc 0 4) 0) 2)
     (expt (longest-diag1 p b (Loc 1 5) 0) 2)))
                     
(: longest-run : Player Board -> Integer)
;;finds the longest run on the board for a player
(define (longest-run p b)
  (max (max-right p b)
       (max-down p b)
       (max-diag1 p b)
       (max-diag2 p b)))
(check-expect (longest-run 'white
                             (quick-board '-ww-w---w 'w---w-wbb '----w-bbb 'w---w----)) 4)

;;heuristic for long-run
(: long-run : Heuristic)
(define long-run longest-run)

;;sum of all squared runs
(: sum-of-squares : Player Board -> Integer)
(define (sum-of-squares p b)
  (+ (sum-right p b)
     (sum-down p b)
     (sum-diag1 p b)
     (sum-diag2 p b)))

;;heuristic for sum-squares
(: sum-squares : Heuristic)
(define sum-squares sum-of-squares)

;;long run difference
(: lr-difference : Player Board -> Integer)
(define (lr-difference p b)
  (- (long-run p b) (long-run (opponent p) b)))

;;long run difference heuristic
(: long-run-difference : Heuristic)
(define long-run-difference lr-difference)

;;sum of squares difference
(: ss-difference : Player Board -> Integer)
(define (ss-difference p b)
  (- (sum-squares p b) (sum-squares (opponent p) b)))

;;sum of squares heuristic
(: sum-squares-difference : Heuristic)
(define sum-squares-difference ss-difference)

;;finds the winning move
(: winning-move : Player Board (Listof Loc) -> (U Loc Move))
(define (winning-move p b list)
  (match list
      [(cons hd tl) (cond
                      [(has-five? (place-marble (Game b (opponent p) 'place) p hd) p) hd]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'NW 'clockwise) p) (Move hd 'NW 'clockwise)]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'NE 'clockwise) p) (Move hd 'NE 'clockwise)]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'SW 'clockwise) p) (Move hd 'SW 'clockwise)]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'SE 'clockwise) p) (Move hd 'SE 'clockwise)]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'NW 'counterclockwise) p) (Move hd 'NW 'counterclockwise)]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'NE 'counterclockwise) p) (Move hd 'NE 'counterclockwise)]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'SW 'counterclockwise) p) (Move hd 'SW 'counterclockwise)]
                      [(has-five? (twist-quadrant (place-marble (Game b (opponent p) 'place) p hd) 'SE 'counterclockwise) p) (Move hd 'SE 'counterclockwise)]
                      [else (winning-move p b tl)])]
       ['() (error "can't find winning move")]))

;;see if a place-marble can win the game
(: direct-win : Player Board -> (Optional (U Loc Move)))
(define (direct-win p b)
  (cond
    [(< (long-run p b) 4) 'none]
    [(= (long-run p b) 4) (Some (winning-move p b loc-list))]
    [else (error "long-run greater than 4")]))

;;finds all possible moves on board
(: possible-moves : Player Board (Listof Loc) -> (Listof Move))
(define (possible-moves p b list)
  (cond
    [(cons? list) (if (place-marble-ok? (Game b p 'place) (first list))
                      (cons (Move (first list) 'NW 'clockwise)
                            (cons (Move (first list) 'NW 'counterclockwise)
                                  (cons (Move (first list) 'NE 'clockwise)
                                        (cons (Move (first list) 'NE 'counterclockwise)
                                              (cons (Move (first list) 'SW 'clockwise)
                                                    (cons (Move (first list) 'SW 'counterclockwise)
                                                          (cons (Move (first list) 'SE 'clockwise)
                                                                (cons (Move (first list) 'SE 'counterclockwise)
                                                                      (possible-moves p b (rest list))))))))))
                      (possible-moves p b (rest list)))]
    [else '()]))

;;returns list of all possible board moves
(: all-possible : Player Board -> (Listof Move))
(define (all-possible p b)
  (possible-moves p b loc-list))

;;finds the maximum of the given heuristic
(: find-max-of-heuristic : Player Board Heuristic (Listof Move) -> Integer)
(define (find-max-of-heuristic p b h list)
  (cond
    [(cons? list)
     (max (h p (Game-board (twist-quadrant (place-marble (Game b p 'place) p (Move-loc (first list)))
                                           (Move-q (first list))
                                           (Move-dir (first list)))))
          (find-max-of-heuristic p b h (rest list)))]
    [else 0]))
       
;;returns the move that gives the maximum for a heuristic
(: bm : Player Board Heuristic (Listof Move) -> Move)
(define (bm p b h list)
  (cond
    [(cons? list)
     (if (= (find-max-of-heuristic p b h list)
            (h p (Game-board (twist-quadrant (place-marble (Game b p 'place) p (Move-loc (first list)))
                                           (Move-q (first list))
                                           (Move-dir (first list))))))
         (first list)
         (bm p b h (rest list)))]
    [else (error "empty list")]))

;;change a loc input to a move
(: change-to-move : (Optional (U Loc Move)) -> Move)
(define (change-to-move ulm)
  (cond
    [(Loc? ulm) (Move ulm 'NW 'clockwise)]
    ['none (error "invalid move")]
    [else ulm]))

;;ai for pentago
(: make-mind : Player Heuristic -> (Game (Listof Loc) -> (Optional Move)))
(define (make-mind p h)
  (local
    {(: best-move : Game (Listof Loc) -> (Optional Move))
     (define (best-move g list)
       (match (direct-win p (Game-board g))
         ['none (Some (bm p (Game-board g) h (all-possible p (Game-board g))))]
         ;[(Loc? (direct-win p (Game-board g))) (Move (direct-win p (Game-board g)) 'NW 'clockwise)]
         [_ (Some (change-to-move (direct-win p (Game-board g))))]))}
    best-move))
       
       

(test)