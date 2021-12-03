;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Day1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Day 1: Sonar Sweep

;; Part 1

;; Input list text file : https://adventofcode.com/2021/day/1/input

;; A [NEList-of X] is one of:
;; - (cons X '())
;; - (cons X [NEList-of X])
;; Represents a list of x with at least one element in this list
(define NEL-1 (list 1))
(define NEL-2 (list 1 2 3))
(define NEL-3 (list "A" "B"))

(define (nel-temp nel)
  (...
   (cond [(empty? (rest nel)) ... (first nel) ...]
         [(cons? (rest nel)) ... (first nel) ...
                             (nel-temp (rest nel)) ...])))

;; count-times-increased : [NEList-of String] -> Nat
;; counts the number of times a depth measurement increases
;; from the previous measurement
(check-expect (count-times-increased (list 1)) 0)
(check-expect (count-times-increased (list 10 30 25 26 25 27 50 40)) 4)

(define (count-times-increased lom)
  (local [;; count-times/acc : [List-of Number] Number Nat -> Nat
          ;; counts the number of times a measurement increases from the previous
          ;; measurement in the list
          ;; ACCUMULATOR : previous keeps track of the previous measurement in the list
          (define (count-times/acc lom previous)
            (cond [(empty? lom) 0]
                  [(cons? lom)
                   (if (> (- (first lom) previous) 0)
                       (add1 (count-times/acc (rest lom) (first lom)))
                       (count-times/acc (rest lom) (first lom)))]))]
    (count-times/acc (rest lom) (first lom))))

(count-times-increased (map string->number (read-words "Input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 2

;; A [NEList-of-3-or-more X] is one of:
;; - (cons X (cons X (cons X '())))
;; - (cons X [NEList-of-3-or-more X])
;; Represents a list of x that contains at least 3 elements
(define NELO3OM-1 (list 1 2 3))
(define NELO3OM-2 (list "A" "B" "C" "D"))

(define (nelo3om-temp nelo3om)
  (...
   (cond [(empty? (rest (rest (rest nelo3om))))
          ... (first nelo3om) ...
          ... (first (rest nelo3om)) ...
          ... (first (rest (rest nelo3om))) ...]
         [(cons? (rest (rest (rest nelo3om))))
          ... (first nelo3om) ... (nelo3om-temp (rest nelo3om)) ...])))

;; count-increased-windows : [NEList-of-3-or-more String] -> Nat
;; counts the number of times a window of three depth measurements increases from the
;; previous window
(check-expect (count-increased-windows (list 5 10 12 30 10 3 15 5)) 1)
(check-expect (count-increased-windows (list 1 2 3)) 0)

(define (count-increased-windows lom)
  (local [;; build-windows : [List-of Number] -> [List-of Number]
          ;; produces a list of numbers where each number is the sum of three
          ;; consecutive measurements from the list
          ;; ACCUMULATOR : p (previous) keeps track of the previous number in the list and
          ;;               pp (previous previous) keeps track of the number previous to the previous
          ;;               number in the list
          (define (build-windows lon p pp)
            (cond [(empty? lon) '()]
                  [(cons? lon) (cons (+ (first lon) p pp)
                                     (build-windows (rest lon) (first lon) p))]))]
    (count-times-increased
     (build-windows (rest (rest lom)) (first (rest lom)) (first lom)))))

(count-increased-windows (map string->number (read-words "Input.txt")))








