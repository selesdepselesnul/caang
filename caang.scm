#!/usr/bin/env ./runscheme.sh
;; -*- geiser-scheme-implementation: 'chicken -*-
(require-extension srfi-13)
(require-extension extras)
(require-extension utils)

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

(define (set-brigthness! value)
  (with-output-to-file
      backlight-brigthness-file 
    (lambda ()
      (format #t value))))

(let ((args (command-line-arguments)))
  (if (null? args)
      (print (get-brigthness-perc))
      (set-brigthness! (car args))))
