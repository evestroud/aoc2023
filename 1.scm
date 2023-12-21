(use-modules (ice-9 textual-ports)
             (ice-9 string-fun)
             (ice-9 pretty-print)
             (srfi srfi-1))

(define (get-input filename)
  (call-with-input-file filename
    (lambda (port) (string-split (get-string-all port) #\newline))))

(define (day1-1 filename)
  (let* ((input (get-input filename))
         (cleaned-input (map replace-text-with-numbers input))
         (calibration-numbers (map get-calibration-number cleaned-input)))
    (fold + 0 calibration-numbers)))

(define numbers '(("zero" "zero0zero")
                  ("one" "one1one")
                  ("two" "two2two")
                  ("three" "three3three")
                  ("four" "four4four")
                  ("five" "five5five")
                  ("six" "six6six")
                  ("seven" "seven7seven")
                  ("eight" "eight8eight")
                  ("nine" "nine9nine")))

(define (replace-text-with-numbers line)
  (fold (lambda (number line)
          (string-replace-substring line (first number) (second number)))
        line
        numbers))

(define (get-numbers line)
  (string-fold
   (lambda (cur prev)
     (if (char-set-contains? char-set:digit cur)
         (cons cur prev)
         prev))
   '()
   line))

(define (get-calibration-number line)
  (let* ((numbers (get-numbers line))
         (calibration-string (if (not (null? numbers))
                                 (string (last numbers) (first numbers))
                                 "0")))
    (string->number calibration-string)))
