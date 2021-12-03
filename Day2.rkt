;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Day2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Day 2: Dive!

;; Part 1

;; Input list text file : https://adventofcode.com/2021/day/2/input

;; A Position is a (make-posn Number Number)
;; Represents the position of the submarine where:
;; - x is the horizontal position of the submarine and
;; - y is the depth of the submarine
(define POS-1 (make-posn 5 10))
(define POS-2 (make-posn 10 30))

#;(define (posn-temp posn)
    (... (posn-x posn) ...
         (posn-y posn) ...))

;; A Direction is one of:
;; - "forward"
;; - "up"
;; - "down"
;; represents the direction of a movement of the submarine

;; An Instructions is one of:
;; - '()
;; - (append (list Direction Number) Instructions)
;; Represents the list of movements to be made by the submarine. Each element in the list
;; is either a Direction or a Number where the Number represents the distance to be
;; moved in the direction specified by the previous element in the list.

;; locate-position : [List-of String] -> Position
;; produces the position of the submarine after it has moved according to the list of instructions.
;; the initial horizontal and depth of the submarine are 0.
(check-expect (locate-position (list "forward" "5" "down" "3" "up" "2")) (make-posn 5 1))
(check-expect (locate-position '()) (make-posn 0 0))

(define (locate-position los)
  (local [;; (converts strings containing numbers in txt file to numbers)
          (define INSTRUCTIONS (map (λ(str) (if (number? (string->number str))
                                                (string->number str)
                                                str))
                                    los))
          ;; locate/acc : Instructions Position -> Position
          ;; produces the position of the submarine after it has moved according
          ;; to the list of instructions.
          ;; ACCUMULATOR : current keeps track of the current position of the submarine
          (define (locate/acc instr current)
            (cond [(empty? instr) current]
                  [(cons? instr) (locate/acc (rest (rest instr))
                                             (update-posn (first instr)
                                                          (first (rest instr))
                                                          current))]))
          ;; update-posn : Direction Number Position -> Position
          ;; produces a new position with the given direction updated by the
          ;; given distance
          (define (update-posn direction distance current)
            (cond [(string=? "forward" direction)
                   (make-posn (+ distance
                                 (posn-x current))
                              (posn-y current))]
                  [(string=? "up" direction)
                   (make-posn (posn-x current)
                              (- (posn-y current)
                                 distance))]
                  [(string=? "down" direction)
                   (make-posn (posn-x current)
                              (+ distance
                                 (posn-y current)))]))]
    (locate/acc INSTRUCTIONS (make-posn 0 0))))

(locate-position (read-words "Input2.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 2

(define-struct status [x y aim])
;; A Status is a (make-status Number Number Number)
;; represents the status of the submarine where
;; - x is its horizontal position
;; - y is its depth
;; - aim is the submarine's aim

;; locate-position.v2 : [List-of String] -> Status
;; produces the position of the submarine after it moves according to the list of instructions.
;; Note: Interpretation of commands has changed since Part 1
(check-expect (locate-position.v2 '()) (make-status 0 0 0))
(check-expect (locate-position.v2 (list "forward" "2" "up" "3" "forward" "1")) (make-status 3 -3 -3))

(define (locate-position.v2 los)
  (local [(define INSTRUCTIONS (map (λ(str) (if (number? (string->number str))
                                                (string->number str)
                                                str))
                                    los))
          ;; locate.v2/acc : Instructions Status Number -> Status
          ;; produces the position of the submarine after it moves according to the instructions
          ;; ACCUMULATOR : current tracks the current status of the submarine
          (define (locate.v2/acc instr current)
            (cond [(empty? instr) current]
                  [(cons? instr) (locate.v2/acc (rest (rest instr))
                                                (update-posn.v2 (first instr)
                                                                (first (rest instr))
                                                                current))]))
          ;; update-posn.v2 : String Number Status -> Status
          (define (update-posn.v2 movement distance current)
            (cond [(string=? "forward" movement)
                   (make-status (+ distance
                                   (status-x current))
                                (+ (* (status-aim current) distance)
                                   (status-y current))
                                (status-aim current))]
                  [(string=? "up" movement)
                   (make-status (status-x current)
                                (status-y current)
                                (- (status-aim current)
                                   distance))]
                  [(string=? "down" movement)
                   (make-status (status-x current)
                                (status-y current)
                                (+ (status-aim current)
                                   distance))]))]
    (locate.v2/acc INSTRUCTIONS (make-status 0 0 0))))

(locate-position.v2 (read-words "Input2.txt"))
            








          