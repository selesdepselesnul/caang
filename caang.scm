#!/usr/bin/env ./runscheme.sh
;; -*- geiser-scheme-implementation: 'chicken -*-
(require-extension srfi-13)
(require-extension extras)
(require-extension utils)
(require-extension regex)

(define backlight-path
  "/sys/class/backlight/intel_backlight/")

(define backlight-brigthness-file
  (string-append backlight-path "brightness"))

(define max-brigthness-file
  (string-append backlight-path "max_brightness"))

(define (read-param)
  (car (command-line-arguments)))

(define (read-all-trim path)
  (string-trim-both (read-all path)))

(define (get-max-brigthness)
  (string->number
   (read-all-trim max-brigthness-file)))

(define (get-brigthness)
  (string->number
   (read-all-trim backlight-brigthness-file)))

(define (get-brigthness-perc) 
  (* (/ (get-brigthness)
        (get-max-brigthness))
     100))

(define (round-exact x)
  (inexact->exact
   (round x)))

(define (calc-actual-brigtness x)
  (inexact->exact
   (round
    (/ (* x (get-max-brigthness))
       100))))

(define (set-brigthness! value)
  (with-output-to-file
      backlight-brigthness-file 
    (lambda ()
      (format #t
              (number->string
               (calc-actual-brigtness
                (string->number value)))))))

(define (is-add-pattern? x)
  (string-match "^\\+[0-9]+$" x))

(define (is-sub-pattern? x)
  (string-match "^\\-[0-9]+$" x))

(define (is-num-pattern? x)
  (string-match "^[0-9]+$" x))

(define (extract-num x)
  (string->number
   (string-substitute
    "^(\\+|\\-)([0-9]+)$"
    "\\2"
    x)))

(define (run-if-range-valid! x f)
  (let ((arg-num (string->number x)))
    (if (and (> arg-num 0) (<= arg-num 100))
        (f x)
        (print "must be in valid range 1..100"))))

(define (adjust-brigthness! f brigthness)
  (let ((val (number->string
              (f (get-brigthness-perc)
                 (extract-num brigthness)))))
    (run-if-range-valid! val
                         (lambda (x) (set-brigthness! x)))))

(define (run! args)
  (if (null? args)
      (print (round-exact
              (get-brigthness-perc)))
      (let ((arg (car args)))
        (cond
         ((is-num-pattern? arg)
          (run-if-range-valid! arg (lambda (x) (set-brigthness! x))))
         ((is-add-pattern? arg)
          (adjust-brigthness! + arg))
         ((is-sub-pattern? arg)
          (adjust-brigthness! - arg))
         (else
          (print "format doesn't valid, valid ex : +2, -2, or 2"))))))

(run! (command-line-arguments))

