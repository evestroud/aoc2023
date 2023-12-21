(use-modules (ice-9 textual-ports)
             (ice-9 string-fun)
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

;; Input

(define verbose #f)

(define (apply-to-input filename . funcs)
  (stream->list (apply apply-to-input-stream (cons filename funcs))))

(define (apply-to-input-stream filename . funcs)
  (define (input-as-stream filename)
    (let* ((port (open-file filename "r")))
      (cons (port->stream port get-line) (lambda () (close-input-port port)))))
  (define (apply-to-line line . funcs)
    (fold (lambda (f line)
            (if verbose (println line) '())
            (f line))
          line
          funcs))
  (let* ((stream-with-close (input-as-stream filename))
         (stream (car stream-with-close))
         (close-file (cdr stream-with-close)))
    (stream-map (lambda (line)
                  (if (not (eof-object? line))
                      (apply apply-to-line (cons line funcs))
                      (close-file)))
                stream)))

;;

(define (get-game-info line)
  (define (get-cubes-from-draw draw)
    (let ((cubes-raw (string-split draw #\,)))
      (map (lambda (cube)
             (let ((color (get-word-from-string cube))
                   (number (get-number-from-string cube)))
               (cons color number)))
           cubes-raw)))
  (let* ((game (string-split line #\:))
         (game-number (get-number-from-string (first game)))
         (draws (string-split (second game) #\;))
         (cubes (map get-cubes-from-draw draws)))
    (cons game-number cubes)))

(define (get-word-from-string line)
  (string-filter char-set:letter line))

(define (get-number-from-string line)
  (string->number (string-filter char-set:digit line)))

;;

(define (get-max-cubes-from-game game)
  (let ((game-number (car game))
        (draws (cdr game)))
    (cons game-number
          (fold update-max-cubes-from-draw
                '()
                draws))))

(define (update-max-cubes-from-draw draw max-cubes)
  (fold (lambda (color-set updated-max-cubes)
          (let* ((color (car color-set))
                 (number (cdr color-set)))
            (acons-if (lambda (new-value old-value)
                        (> new-value old-value))
                      color
                      number
                      updated-max-cubes)))
        max-cubes
        draw))

(define (acons-if pred key value alist)
  (define (update-entry-if entry alist-copy)
    (let ((entry-key (car entry))
          (entry-value (cdr entry)))
      (if (and (equal? key entry-key) (pred value entry-value))
          (acons key value alist-copy)
          (acons entry-key entry-value alist-copy))))
  (if (not (assoc key alist))
      (acons key value alist)
      (fold
       update-entry-if
       '()
       alist)))

;;

(define (filter-impossible-games game)
  (define max-cubes '(("red" . 12) ("green" . 13) ("blue" . 14)))
  (let ((game-id (car game))
        (max-cubes-for-game (cdr game)))
    (fold (lambda (cubes prev-result)
            (let* ((color (car cubes))
                  (number (cdr cubes))
                  (max-possible (or (cdr (assoc color max-cubes)) 0)))
              (if (> number max-possible)
                  #f
                  prev-result)))
          #t
          max-cubes-for-game)))

;; Part 1 solution
;; (fold + 0 (map car (filter filter-impossible-games (apply-to-input "2.txt" get-game-info get-max-cubes-from-game))))
;; 2105

(define (power-of-cubes game)
  (let ((cubes (cdr game)))
    (fold (lambda (cube result)
            (* result (cdr cube)))
          1
          cubes)))

;; Part 2 solution
;; (fold + 0 (apply-to-input "2.txt" get-game-info get-max-cubes-from-game power-of-cubes))
