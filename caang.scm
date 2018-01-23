;; -*- geiser-scheme-implementation: 'chicken -*-
(require-extension srfi-13)
(require-extension extras)
(require-extension utils)
(require-extension regex)
(require-extension ansi-escape-sequences)
(require-extension fmt)
(require-extension posix)

(define backlight-path "/sys/class/backlight")
(define default-pref-file ".caang_default")

(define (get-vendor-backlight)
  (car (directory backlight-path)))

(define (get-full-backlight-path x)
  (string-append backlight-path
                 "/"
                 (get-vendor-backlight)
                 "/"
                 x))

(define backlight-brigthness-file
  (get-full-backlight-path "brightness"))

(define max-brigthness-file
  (get-full-backlight-path "max_brightness"))

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
  (run-if-range-valid!
   value
   (lambda (x)
     (begin
       (with-output-to-file backlight-brigthness-file
         (lambda ()
           (format #t
                   (number->string
                    (calc-actual-brigtness
                     (string->number x))))))
       (print-current-brigthness-perc)))))

(define (is-add-pattern? x)
  (string-match "^\\+[0-9]+$" x))

(define (is-sub-pattern? x)
  (string-match "^\\-[0-9]+$" x))

(define (is-num-pattern? x)
  (string-match "^[0-9]+$" x))

(define (extract-num x)
  ;; Extract number part only from string x and convert it to number
  ;; ex : "+4" will return 4
  (string->number
   (string-substitute
    "^(\\+|\\-)([0-9]+)$"
    "\\2"
    x)))

(define (run-if-range-valid! x f)
  ;; run function f with supplied arg x if only in valid range
  ;; (and (>= x 1) (<= x 100))
  (let ((arg-num (string->number x)))
    (if (and (>= arg-num 1) (<= arg-num 100))
        (f x)
        (print "must be in valid range 1..100"))))

(define (adjust-brigthness! f brigthness)
  (let ((val (number->string
              (f (get-brigthness-perc)
                 (extract-num brigthness)))))
    (set-brigthness! val)))

(define (adjust-live!)
  (let ((current-brigthness (number->string
                             (round-exact
                              (get-brigthness-perc)))))
    (print (fmt #f "Current brigthness: " current-brigthness nl)) 
    (choose-adjust-type! (read-line)) 
    (adjust-live!)))

(define (choose-adjust-type! x)
  (cond
   ((is-num-pattern? x)
    (set-brigthness! x))
   ((is-add-pattern? x)
    (adjust-brigthness! + x))
   ((is-sub-pattern? x)
    (adjust-brigthness! - x))
   (else
    (print "format doesn't valid, valid ex : +2, -2, or 2"))))

(define (print-current-brigthness-perc)
  (print
   (string-append
    (number->string (round-exact
                     (get-brigthness-perc)))

    "%")))

(define (trim-nl x)
  (string-substitute
      "\n"
      ""
      x))

(define (get-home-dir)
  "Get current $HOME dir"
  (string-append
    (trim-nl
      (call-with-input-pipe
       "echo $HOME"
       read-all))
    "/"))

(define (get-home-dir-when-sudo)
  (string-append
    "/home/"
    (trim-nl
      (call-with-input-pipe
        "echo $SUDO_USER"
        read-all))
    "/"))

(define (get-default-pref-path)
  (string-append (get-home-dir)
                 ".caang_default"))

(define (save-default! x)
  "Save default brigthness preferences value in $HOME/.caang_default"
  (with-output-to-file
    (get-default-pref-path)
    (lambda ()
      (format #t x))))

(define (run! args)
  (if (null? args) 
      (print-current-brigthness-perc)
      (let ((arg (car args)))
        (cond
         ((string-ci= arg "--live")
          (adjust-live!))
         ((string-ci= arg "--min")
          (set-brigthness! "1"))
         ((string-ci= arg "--max")
          (set-brigthness! "100"))
         ((string-ci= arg "--save-default")
          (run-if-range-valid! (cadr args)
                               save-default!))
         ((string-ci= arg "--default")
          (set-brigthness! (read-all
                             (string-append
                               (get-home-dir-when-sudo)
                               default-pref-file))))
         (else
          (choose-adjust-type! arg))))))

(handle-exceptions exn
                   (begin
                     (display "permission denied !")
                     (newline))
  (run! (command-line-arguments)))

