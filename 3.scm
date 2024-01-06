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

(define-once verbose #f)
(define-once (toggle-verbose) (not verbose))

;; Input

(define (get-input filename)
  (let ((input (get-string-all (open-input-file filename))))
    (string-trim-both input)))

(define (process-input input)
  (list->vector
   (map list->vector
        (map string->list
             (string-split input #\newline)))))

(define (get-by-index grid y x)
  (if (or (< y 0) (< x 0)
          (>= y (grid-col-size grid))
          (>= x (grid-row-size grid)))
      #f
      (vector-ref (vector-ref grid y) x)))

(define (grid-row-size grid) (vector-length (vector-ref grid 0)))
(define (grid-col-size grid) (vector-length grid))

; Part One

                                        ; Outer: grid traversal
(define (traverse-grid grid y x result)
  (if (>= x (grid-row-size grid)) (traverse-grid grid (1+ y) 0 result)
      (if (>= y (grid-col-size grid)) result
          (let ((value (get-by-index grid y x)))
            (if (char-set-contains? char-set:digit value)
                (consume-number grid y x '() #f result)
                (traverse-grid grid y (1+ x) result))))))

                                        ; Inner: consume and validate number
(define (consume-number grid y x number-part valid? result)
  (let ((value (get-by-index grid y x)))
    (if (and value (char-set-contains? char-set:digit value))
        (let ((is-square-valid? (validate-square grid y x)))
          (consume-number grid y (1+ x) (cons value number-part) (or valid? is-square-valid?) result))
        (let ((number (make-number number-part)))
          (traverse-grid grid y x (if valid?
                                    (cons number result)
                                    result))))))

(define (validate-square grid y x)
  (define to-check-y (list (1- y) y (1+ y)))
  (define to-check-x (list (1- x) x (1+ x)))
  (define symbols (char-set-delete
                   (char-set-union char-set:symbol char-set:punctuation)
                   #\.))
  (fold (lambda (y valid?)
          (or valid?
              (fold (lambda (x valid?)
                      (let ((value (get-by-index grid y x)))
                        (or valid? (and value (char-set-contains? symbols value)))))
                    valid?
                    to-check-x)))
        #f
        to-check-y))

(define (make-number number-part)
  (string->number (string-reverse (apply string number-part))))

; Solution: (fold + 0 (traverse-grid (process-input (get-input "3.txt")) 0 0 '()))

; Part Two

; Iterate over data until a '*' is found

(define (traverse-grid-generic predicate consequent grid)
  (define (helper y x result)
    (if (>= x (grid-row-size grid)) (helper (1+ y) 0 result)
        (if (>= y (grid-col-size grid)) result
            (let* ((value (get-by-index grid y x))
                   (result (if (predicate value)
                               (cons (consequent grid y x) result)
                               result)))
              (helper y (1+ x) result)))))
  (helper 0 0 '()))

(define (is-gear value)
  (eq? value #\*))

; If a gear touches two numbers exactly, then parse those numbers

; identify whether the * contacts two numbers exactly
;   digits on separate lines must be part of different numbers
;   digits with a . (or * on the middle row) between them must be part of different numbers
;   any contiguous digits above or below must be part of the SAME number

(define (get-gear-ratio grid y x)
  (let ((number-coords (get-number-coords grid y x)))
    (if (= 2 (length number-coords))
        (apply * (map (lambda (coord) (get-number grid coord)) number-coords))
        0)))

;   if only one digit seen (corners or to immediate left or right of gear) then traverse left/right
;   based on which side it's located on to find all its digits
;   if two digits from same number seen, find the end of the seen number and traverse away from the end
;   if three digits from same number seen, already have full number (max three digits in sample set)

(define (get-number grid coord)
  (let* ((y (car coord))
         (x (cdr coord)))
    (string->number (apply string (append (search-left grid y x '()) (search-right grid y (1+ x)))))))

(define (search-left grid y x accumulator)
  (let ((value (get-by-index grid y x)))
    (if (or (not value) (not (is-digit value)))
        accumulator
        (search-left grid y (1- x) (cons value accumulator)))))

(define (search-right grid y x)
  (let ((value (get-by-index grid y x)))
    (if (or (not value) (not (is-digit value)))
        '()
        (cons value (search-right grid y (1+ x))))))

(define (get-number-coords grid y x)
  (define to-check-y (list (1- y) y (1+ y)))
  (define to-check-x (list (1- x) x (1+ x)))
  (append-map
   (lambda (y )
     (fold
      (lambda (x line)
        (let* ((value (get-by-index grid y x))
               (previous (and (not (null? line)) (car line)))
               (x-prev (and previous (cdr previous))))
          (if (is-digit value)
              (if (eq? (1- x) x-prev) ; don't add digits if contiguous
                  (cons (cons y x) (cdr line))
                  (cons (cons y x) line))
              line)
          ))
      '()
      to-check-x
      ))
   to-check-y))

(define (is-digit value)
  (char-set-contains? char-set:digit value))

; Solution

(define (part-two grid)
  (traverse-grid-generic is-gear get-gear-ratio grid))

; (apply + (part-two (process-input (get-input "3.txt"))))
