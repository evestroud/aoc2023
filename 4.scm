(use-modules (ice-9 textual-ports)
             (ice-9 pretty-print)
             (ice-9 streams)
             (srfi srfi-1))

;; Display helpers

(define (auto-format . args)
  (let ((format-string (string-join (make-list (length args) "~a"))))
    (apply format (append (list #f format-string) args))))

(define (print . args)
  (display (apply auto-format args)))

(define (println . args)
  (apply print args)
  (newline))

(define (debug . args)
  (apply println args)
  (last args))


;; Input

(define (get-input filename)
  (let ((input (get-string-all (open-input-file filename))))
    (string-trim-both input)))

(define (process-input processor input)
  (map processor
       (string-split input #\newline)))

(define (process-line line)
  (let* ((card (string-split (second (string-split line #\:)) #\|))
         (winning (clean-and-split-numbers (first card)))
         (you-have (clean-and-split-numbers (second card))))
    (cons winning (list you-have))))

(define (clean-and-split-numbers raw-string)
  (map string->number
       (filter (lambda (x) (not (equal? x "")))
               (string-split raw-string #\space))))

(define sample (process-input process-line (get-input "4.1.txt")))
(define input (process-input process-line (get-input "4.txt")))


; Part 1

; Iterate over held numbers and test for presence in winning list
; Sets?

(define (winning-numbers-in-card card)
  (let ((winning (first card))
        (in-hand (second card)))
    (length (lset-intersection eqv? winning in-hand))))

(define (score-card card)
  (round (expt 2 (1- card))))

; Solution (fold + 0 (map score-card (map winning-numbers-in-card input)))
