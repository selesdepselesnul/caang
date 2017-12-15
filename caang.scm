#!/usr/bin/env ./runscheme.sh
;; -*- geiser-scheme-implementation: 'chicken -*-
(define backlight-brigthness "/sys/class/backlight/intel_backlight/brightness")

(define (read-param)
  (car (command-line-arguments)))

(with-output-to-file
    backlight-brigthness
  (lambda ()
    (format #t (read-param) 1)))


