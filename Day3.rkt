;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Day3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Day 3: Binary Diagnostic

;; Input list text file : https://adventofcode.com/2021/day/3/input

;; Part 1

;; INSTRUCTIONS

;; Each bit in the gamma rate can be determined by finding the most common bit in the
;; corresponding position of all numbers in the diagnostic report.
;; The epsilon rate is calculated in a similar way; rather than use the most common bit,
;; the least common bit from each position is used.

;; binary->decimal : BinaryString -> Number
;; converts the non-two's-complement binary number to decimal
(check-expect (binary->decimal "1001") 9)

(define (binary->decimal binstr)
  (local [(define EXPLODED (explode binstr))
          ;; bin->dec : [List-of BinaryString] Number -> Number
          ;; converts the exploded binary number to decimal
          ;; ACCUMULATOR : decimal keeps track of the sum of the decimal numbers represented
          ;;               by each bit
          (define (bin->dec binstr decimal)
            (cond [(empty? binstr) decimal]
                  [(cons? binstr)
                   (bin->dec (rest binstr)
                             (if (string=? (first binstr) "1")
                                 (+ (expt 2 (- (length binstr) 1))
                                    decimal)
                                 decimal))]))]
    (bin->dec EXPLODED 0)))

;; A BinaryString is a String containing a Binary Number
;; Examples:
;; - "00011101010"
;; - "0011"
;; - "11001100"

;; gamma-rate : [List-of BinaryString] -> BinaryString
;; produces the binary number of the gamma rate of the submarine
(define (gamma-rate los)
  (local [;; (separates the BinaryStrings in the list into a list of single-bit BinaryStrings)
          (define EXPLODED (map explode los))
          ;; most-commmon : [List-of [List-of BinaryString]] -> BinaryString
          ;; calculates the most common first digit in a list of binary numbers
          (define (most-common lolos count0 count1)
            (cond [(empty? lolos) (if (> count0 count1)
                                      "0" "1")]
                  [(cons? lolos)
                   (if (string=? "0" (first (first lolos)))
                       (most-common (rest lolos) (add1 count0) count1)
                       (most-common (rest lolos) count0 (add1 count1)))]))
          ;; traverse-num : [List-of [List-of BinaryString]] -> BinaryString
          ;; computes the binary number of the gamma rate of the submarine
          (define (traverse-num lolos)
            (cond [(empty? (first lolos)) ""]
                  [(cons? (first lolos))
                   (string-append (most-common lolos 0 0)
                                  (traverse-num (map rest lolos)))]))]
    (traverse-num EXPLODED)))

(binary->decimal (gamma-rate (read-words "Input3.txt")))

;; epsilon-rate : BinaryString -> BinaryString
;; computes the epsilon rate given the gamma rate (flips the bits)
(define (epsilon-rate gamma)
  (implode
   (map (λ(str) (if (string=? str "0") "1" "0")) (explode gamma))))

(binary->decimal (epsilon-rate (gamma-rate (read-words "Input3.txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 2

;; INSTRUCTIONS

;; To find oxygen generator rating, determine the most common value (0 or 1) in the
;; current bit position, and keep only numbers with that bit in that position.
;; If 0 and 1 are equally common, keep values with a 1 in the position being considered.
;; To find CO2 scrubber rating, determine the least common value (0 or 1) in the current
;; bit position, and keep only numbers with that bit in that position.
;; If 0 and 1 are equally common, keep values with a 0 in the position being considered.

;; If you only have one number left, stop; this is the rating value for which you are searching.
;; Otherwise, repeat the process, considering the next bit to the right.


;; list-tail : [List-of Any] Int -> [List-of Any]
;; produces the section of the list from the given index to the end of the list
(check-expect (list-tail (list 1 2 3 4 5) 3) (list 4 5))

(define (list-tail list index)
  (local [(define original-length (length list))
          (define (list-tail/recur list)
            (cond [(= index (- original-length (length list))) list]
                  [else (list-tail/recur (remove (first list) list))]))]
    (list-tail/recur list)))

;; oxygen-rating : [List-of BinaryString] -> BinaryString
;; computes the oxygen rating of the submarine according to the guidelines
(define (oxygen-rating los)
  (local [;; (separates the BinaryStrings in the list into a list of single-bit BinaryStrings)
          (define EXPLODED (map explode los))
          ;; oxygen/acc : [List-of [List-of BinaryString]] Integer -> BinaryString
          ;; computes the oxygen rating
          ;; ACCUMULATOR : index keeps track of the current bit position
          (define (oxygen/acc lolos index)
            (cond [(= 1 (length lolos)) (implode (first lolos))]
                  [else
                   (oxygen/acc (if (string=? (most-common lolos 0 0 index) "0")
                                   (filter (λ(los) (string=? (list-ref los index) "0"))
                                           lolos)
                                   (filter (λ(los) (string=? (list-ref los index) "1"))
                                           lolos))
                               (add1 index))]))
          ;; most-commmon : [List-of [List-of BinaryString]] Integer Integer Integer -> BinaryString
          ;; calculates the most common first digit in a list of binary numbers
          ;; ACCUMULATORS : count0 keeps track of the amount of first digits that are zeros
          ;;                count1 keeps track of the amount of first digits that are ones
          (define (most-common lolos count0 count1 index)
            (cond [(empty? lolos) (if (> count0 count1)
                                      "0" "1")]
                  [(cons? lolos)
                   (if (string=? "0" (list-ref (first lolos) index))
                       (most-common (rest lolos) (add1 count0) count1 index)
                       (most-common (rest lolos) count0 (add1 count1) index))]))]
    (oxygen/acc EXPLODED 0)))

(binary->decimal (oxygen-rating (read-words "Input3.txt")))


;; co2-rating : [List-of BinaryString] -> BinaryString
;; computes the co2 rating of the submarine according to the guidelines
(define (co2-rating los)
  (local [;; (separates the BinaryStrings in the list into a list of single-bit BinaryStrings)
          (define EXPLODED (map explode los))
          ;; co2/acc : [List-of [List-of BinaryString]] Integer -> BinaryString
          ;; computes the co2 rating
          ;; ACCUMULATOR : index keeps track of the current bit position
          (define (co2/acc lolos index)
            (cond [(= 1 (length lolos)) (implode (first lolos))]
                  [else
                   (co2/acc (if (string=? (least-common lolos 0 0 index) "0")
                                (filter (λ(los) (string=? (list-ref los index) "0"))
                                        lolos)
                                (filter (λ(los) (string=? (list-ref los index) "1"))
                                        lolos))
                            (add1 index))]))
          ;; least-common : [List-of [List-of BinaryString]] Nat Nat -> BinaryString
          ;; computes the least common first digit in a list of binary numbers
          ;; ACCUMULATORS : count0 keeps track of the amount of first digits that are zeros
          ;;                count1 keeps track of the amount of first digits that are ones
          (define (least-common lolos count0 count1 index)
            (cond [(empty? lolos) (if (<= count0 count1)
                                      "0" "1")]
                  [(cons? lolos)
                   (if (string=? "0" (list-ref (first lolos) index))
                       (least-common (rest lolos) (add1 count0) count1 index)
                       (least-common (rest lolos) count0 (add1 count1) index))]))]
    (co2/acc EXPLODED 0)))

(binary->decimal (co2-rating (read-words "Input3.txt")))






