#lang racket/base

(require quickscript
         racket/gui)

;;;
;;; BREAKOUT
;;;

; Jens Axel Søgaard, Feb 2014, https://github.com/soegaard
;     https://github.com/soegaard/breakout

;;; License: MIT
;;; From: https://github.com/Quickscript-Competiton/July2020entries/issues/4

;;; References
;;;   http://en.wikipedia.org/wiki/Breakout_%28video_game%29

(script-help-string "Breakout game.
Move: left and right arrows. New ball: b. Reset: r.")


;;; Data Representation
(struct world       (paddle bricks ball)   #:transparent)
(struct body        (x y w h)              #:transparent)
(struct brick  body (strength)             #:transparent)
(struct ball   body (vx vy)                #:transparent)
(struct paddle body (dead?)                #:transparent)

; The velocities vx and vy of the ball are measured in pixels pr second.

;;; Configuration
(define paddle-width      30)
(define paddle-height      6)
(define ball-size          3)
(define brick-width       30)
(define brick-height      10)
(define bricks-in-a-row   10)
(define brick-in-a-column  5)
(define brick-gap          2)
(define width              (+ (* brick-width (+ bricks-in-a-row 2))
                              (* brick-gap   (- bricks-in-a-row 2))))
(define height           200)


(define frames-per-second 10)

(define Δt (/ 1. frames-per-second))

;;; Smart Constructors

(define (new-ball x y vx vy)
  (ball x y ball-size ball-size vx vy))

(define (new-brick x y s)
  (brick x y brick-width brick-height s))

(define (new-paddle x y)
  (paddle x y paddle-width paddle-height #f))

;;;
;;; MODEL
;;;

;;; Creation

; create-world : -> world
;  the initial world contains a paddle and a bunch of bricks
(define (create-world)
  (world (create-paddle) (create-bricks) (create-ball)))

; create-bricks : -> (list body)
;   create list of twenty-four bricks
(define (create-bricks)
  (define w brick-width)
  (define h brick-height)
  (define gap brick-gap)
  (define margin 30)
  (define rows 5)
  (define cols 10)
  (for/list ([i (* rows cols)])
    (define row (quotient  i cols))
    (define strength (- rows row))
    (define x (+ margin (* (+ w gap) (remainder i cols)))) 
    (define y (+ margin (* (+ h gap) row)))
    (new-brick x y strength)))

; create-paddle : -> paddle
(define (create-paddle)
  (define x (- (/ width 2.) (/ paddle-width 2.)))
  (define y (- height (* 2. paddle-height)))
  (new-paddle x y))

(define (create-ball)
  (define x (- (/ width 2.) (/ paddle-width 2.)))
  (define y (- height (* 3. paddle-height)))
  (new-ball x y 4. -20.))  ; velocities in pixels per second

;;; Updaters

; update-brick : brick -> brick
;   nothing happens here yet
(define (update-brick b)
  (match-define (brick x y w h s) b)
  (brick x y w h s))

; update-ball : ball -> ball
#;(define (update-ball b)
    (match-define (ball x y w h vx vy) b)
    (ball (+ x vx) (+ y vy) w h vx vy))

; update-paddle : world -> world
(define (update-paddle w)
  (match-define (world p bricks balls) w)
  (match-define (paddle x y pw ph dead?) p)
  (define moved-paddle
    (cond [dead?              p]
          [(key-down? 'left)  (paddle (- x 2.) y pw ph dead?)]
          [(key-down? 'right) (paddle (+ x 2.) y pw ph dead?)]
          [else               (paddle    x     y pw ph dead?)]))
  (world moved-paddle bricks balls))

;;; UPDATES

(define (update w)
  (new-ball-on-b
   (restart-on-r
    (update-paddle
     (update-bricks
      (update-ball w))))))


(define (update-bricks w)
  (define bs (world-bricks w))
  (struct-copy world w [bricks (map update-brick bs)]))

(define (update-ball w)
  (define b (world-ball w))
  (move-ball w b))

(define (restart-on-r w)
  (if (key-down? #\r)
      (create-world)
      w))

(define (new-ball-on-b w)
  (if (key-down? #\b)
      (struct-copy world w 
                   [ball   (create-ball)]
                   [paddle (create-paddle)])
      w))


;;; Collision

(define (line-intersection x0 y0  x1 y1  x2 y2  x3 y3)
  (struct line-equation (a b c) #:transparent) ; ax+by=c
  (define (two-points->line-equation x0 y0 x1 y1)
    (define a (- y1 y0))
    (define b (- x0 x1))
    (define c (+ (* a x0) (* b y0)))
    (line-equation a b c))
  (define l0 (two-points->line-equation x0 y0 x1 y1))
  (define l1 (two-points->line-equation x2 y2 x3 y3))
  (match-define (line-equation a0 b0 c0) l0)
  (match-define (line-equation a1 b1 c1) l1)
  (define det (- (* a0 b1) (* a1 b0)))
  (cond [(zero? det) #f] ; lines are parallel 
        [else        (list (/ (- (* b1 c0) (* b0 c1)) det)
                           (/ (- (* a0 c1) (* a1 c0)) det))]))

(define (move-ball w b)
  (match-define (ball x y bw bh vx vy) b)
  (define Δx (* Δt vx)) ; distance = time * velocity
  (define Δy (* Δt vy))
  ; the total distance to move during this time step
  (define Δ (sqrt (+ (sqr Δx) (sqr Δy))))
  ; the number of steps: a step needs to so small that
  ; the ball moves at most one pixlel in both the horisontal 
  ; and vertical direction (this way a fast ball can't move through a brick)
  (define n (* 3 (inexact->exact (ceiling (max (abs Δx) (abs Δy) #;(abs Δ))))))
  ; the individual steps in each direction:
  
  ; (displayln (list 'move-ball 'steps n 'Δx Δx 'Δy Δy))
  (match-define (world paddle bricks _) w)
  (for/fold ([w (world paddle bricks b)]) ([_ n])
    ; if (during move-ball/one-step) the ball velocity is changed,
    ; the Δx/n and Δy/n needs to be recomputed - so we can't
    ; just use Δx and Δy from above repeatedly
    (match-define (ball x y bw bh vx vy) (world-ball w))
    (define Δx/n (/ (* Δt vx) n))
    (define Δy/n (/ (* Δt vy) n))
    (move-ball/one-step w Δx/n Δy/n)))

(define (move-ball/one-step w Δx Δy)
  (unless (<= (sqrt (+ (sqr Δx) (sqr Δy))) 1.)
    #;(displayln (list Δx Δy (sqrt (+ (sqr Δx) (sqr Δy)))))
    (error 'move-ball "internal error: expected small steps to be smaller than 1"))
  ; move the first ball in w the distance given by Δx and Δy,
  ; handle collisions: i.e. remove brick and change direction
  (match-define (world paddle bricks b) w)
  (match-define (ball x y bw bh vx vy) b)  
  (define moved-ball (ball (+ x Δx) (+ y Δy) bw bh vx vy))
  
  (handle-ball/wall-collision
   (handle-ball/paddle-collision
    (handle-ball/brick-collisions 
     (world paddle bricks moved-ball)))))

(define (colliding? b1 b2)
  (match-define (body x1 y1 w1 h1) b1)
  (match-define (body x2 y2 w2 h2) b2)
  (not (or (eq? b1 b2)
           (< (+ x1 w1) x2) (> x1 (+ x2 w2))
           (< (+ y1 h1) y2) (> y1 (+ y2 h2)))))

(define (maybe-flip a-ball a-brick)
  ; (displayln (list a-ball a-brick))
  ; a collision between the ball and the body has been detected,
  ; maybe flip the x and y velocities of the ball
  (match-define (body bx by bw bh) a-brick)
  (define (~ x y) (<= (abs (- x y)) 1.5)) ; xxx
  (define (maybe-flip-vx a-ball)
    (match-define (ball x y w h vx vy) a-ball)
    (cond [(~ (+ x w) bx)  (ball (- bx  w) y w h (- vx) vy)]
          [(~ x (+ bx bw)) (ball (+ bx bw) y w h (- vx) vy)]
          [else a-ball]))
  (define (maybe-flip-vy a-ball)
    (match-define (ball x y w h vx vy) a-ball)
    (cond [(~ (+ y h) by)  (ball x (- by h)  w h vx (- vy))]
          [(~ y (+ by bh)) (ball x (+ by bh) w h vx (- vy))]
          [else a-ball]))
  (maybe-flip-vy (maybe-flip-vx a-ball)))

(define (reduce-brick-strength b)
  (match-define (brick x y w h s) b)
  (brick x y w h (max (- s 1) 0)))

(define (handle-ball/brick-collisions w)
  ; given the ball b, remove any bricks colliding with b
  ; if the ball collides with a brick, change its direction
  (match-define (world paddle bricks ball) w)
  (define-values (new-bricks new-ball)
    (for/fold ([new-bricks '()] [ball ball])
              ([brick bricks])
      (cond 
        [(colliding? brick ball)
         (define new-brick (reduce-brick-strength brick))
         (if (zero? (brick-strength new-brick))
              (values                 new-bricks  (maybe-flip ball brick))
              (values (cons new-brick new-bricks) (maybe-flip ball brick)))]
        [else (values (cons brick new-bricks)                 ball)])))
  (world paddle new-bricks new-ball))

(define (handle-ball/paddle-collision w)
  ; handle collisions between ball and paddle
  (match-define (world p bricks b) w)
  (cond 
    [(colliding? p b)
     (match-define (ball    x  y bw bh vx vy) b)
     (match-define (paddle px py pw ph d)     p)
     (define ball-center-x   (/ (+  x  x bw) 2.))
     (define paddle-center-x (/ (+ px px pw) 2.))
     (define v (sqrt (+ (sqr vx) (sqr vy))))
     (define vx* (*    (/ (- ball-center-x  paddle-center-x) (/ pw 2.))))
     (define vy* (* -1 (sqrt (abs (- 1. (sqr vx*))))))
     (world p bricks (ball x y bw bh (* vx* v) (* vy* v)))]
    [else w]))

(define (handle-ball/wall-collision w)
  ; handle collisions between the first ball and the paddle
  (match-define (world paddle bricks a-ball) w)
  (match-define (ball x y bw bh vx vy) a-ball)
  (cond
    ; upper wall
    [(<= y 0)     (world paddle bricks (ball x 1 bw bh vx (- vy)))]
    ; left wall
    [(<= x 0)     (world paddle bricks (ball 1 y bw bh (- vx) vy))]
    ; right wall
    [(>= x width) (world paddle bricks (ball (- width 1) y bw bh (- vx) vy))]
    [else w]))

;;; DRAWING

; draw-bodies : (list body) drawing-context -> void
;   draw the bodies in the world w to the drawing context dc
(define (draw-bodies bs dc)
  (for ([b bs])
    (match-define (body x y w h) b)
    (define c 
      (cond 
        [(paddle? b) (if (paddle-dead? b) "red" "green")]
        [(brick? b)  (match (brick-strength b)
                       [1 "blue"]   [2 "green"] [3 "yellow"]
                       [4 "orange"] [5 "red"]   [_ "black"])]
        [else        "black"]))    
    (send dc set-brush (new brush% [color c] [style 'solid]))
    (send dc draw-rectangle x y w h)))

(define (draw-world w dc)
  (match-define (world paddle bricks ball) w)
  (draw-bodies (append (list paddle) bricks (list ball)) dc))

;;; GUI STATE

(define the-world (create-world))

;;; Keyboard
; The keyboard state is kept in a hash table. 
; Use key-down? to find out, whether a key is pressed or not.
(define the-keyboard (make-hasheq))
(define (key-down! k) (hash-set! the-keyboard k #t))
(define (key-up! k)   (hash-set! the-keyboard k #f))
(define (key-down? k) (hash-ref  the-keyboard k #f))

;;; Canvas
; Key events sent to the canvas updates the information in the-keyboard.
; Paint events calls draw-world. To prevent flicker we suspend flushing
; while drawing commences.
(define game-canvas%
  (class canvas%
    (define/override (on-event e) ; mouse events
      'ignore)
    (define/override (on-char e)  ; key event
      (define key     (send e get-key-code))
      (define release (send e get-key-release-code))
      (when (eq? release 'press)  ; key down?
        (key-down! key))
      (when (eq? key 'release)    ; key up?
        (key-up! release)))
    (define/override (on-paint)   ; repaint (exposed or resized)
      (define dc (send this get-dc))
      (send this suspend-flush)
      (send dc clear)
      (draw-world the-world dc)
      (send this resume-flush))
    (super-new)))

;;---------------------------

(define-script breakout
  #:label "Breakout"
  #:menu-path ("&Games and fun")
  (λ (selection)

    ; Create frame with canvas and show it.
    (define frame  (new frame%  [label "Breakout"]))
    (define canvas (new game-canvas% [parent frame] [min-width width] [min-height height]))
    (send frame show #t)

    ; Start a timer. Each time the timer triggers, the world is updated.
    (define timer (new timer% 
                       [notify-callback 
                        (λ () 
                          (set! the-world (update the-world))
                          (send canvas on-paint))]
                       [interval (inexact->exact (* Δt 1000))])) ; in milliseconds
    (send timer start 20)

    
    #f))
