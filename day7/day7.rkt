#lang racket/base
(require racket/file)
(require racket/function)
(require racket/list)
(require racket/match)
(require racket/string)

(struct hand (cards bid type) #:transparent)

(define (lex-lt? x y)
  (let* ((diff (map - x y))
         (lt (or (index-where diff negative?) 5))
         (gt (or (index-where diff positive?) 5)))
    (< lt gt)
    ))

(define (hand-lt? x y)
  (cond
   ((not (= (hand-type x) (hand-type y))) (< (hand-type x) (hand-type y)))
   (#t (lex-lt? (hand-cards x) (hand-cards y)))
  ))

(define (card->value c)
  (case c
    [(#\A) 14]
    [(#\K) 13]
    [(#\Q) 12]
    [(#\J) 11]
    [(#\T) 10]
    [else (- (char->integer c) (char->integer #\0))]
  ))

(define (cards->type cards)
  (let* ((unique-cards (remove-duplicates cards))
         (counts (map (lambda (c) (count (curry equal? c) cards)) unique-cards)))
    (cond
     ((= (length unique-cards) 1) 7) ; five of a kind
     ((= (length unique-cards) 2)
      (cond
       ((equal? '(1 4) (sort counts <)) 6) ; four of a kind
       ((equal? '(2 3) (sort counts <)) 5) ; full house
       ))
     ((= (length unique-cards) 3)
      (cond
       ((equal? '(1 1 3) (sort counts <)) 4) ; three of a kind
       ((equal? '(1 2 2) (sort counts <)) 3) ; two pair
       ))
     ((= (length unique-cards) 4) 2) ; 1 pair
     ((= (length unique-cards) 5) 1) ; high card
    )
  ))

(define (line->hand l)
  (match (string-split l)
    [(list cs bs) (hand (map card->value (string->list cs))
                        (string->number bs)
                        (cards->type (string->list cs)))]
    ))

(define (part1 lines)
  (let ((hands (sort (map line->hand lines) hand-lt?)))
    (foldl + 0 (map * (map hand-bid hands) (range 1 (+ (length lines) 1))))
  ))

(define (card->value2 c)
  (case c
    [(#\A) 14]
    [(#\K) 13]
    [(#\Q) 12]
    [(#\J) 1]
    [(#\T) 10]
    [else (- (char->integer c) (char->integer #\0))]
  ))

(define (replace-jokers cards)
  (let ((cardf (remove* '(#\J) cards)))
    (cond
     ((null? cardf) (make-list 5 #\A))
     (#t (let ((most-common (argmax (lambda (c) (count (curry equal? c) cardf)) cardf)))
           (append cardf (make-list (count (curry equal? #\J) cards) most-common))
      ))
     )))

(define (line->hand2 l)
  (match (string-split l)
    [(list cs bs) (hand (map card->value2 (string->list cs))
                        (string->number bs)
                        (cards->type (replace-jokers (string->list cs))))]
    ))

(define (part2 lines)
  (let ((hands (sort (map line->hand2 lines) hand-lt?)))
    (foldl + 0 (map * (map hand-bid hands) (range 1 (+ (length lines) 1))))
  ))

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline)
    )
  )
