;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname articles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)
(define file-words (read-words/line "ttt.txt"))
(check-expect (remove-articles file-words) (list
 (list "Why" "does" "girl" "sail?")
 (list "Ships" "endure!")
 (list "Desolation," "life," "and" "courage.")
 (list "Stormy," "dead" "winds" "calmly" "pull" "old," "rough" "mast.")))
(define (article? l)
(cond
  [(or (string=? l"a") (or (string=? l"an") (or (string=? l"the") (or (string=? l"those") (string=? l"these"))))) true]
  [(false? (or (string=? l"a") (or (string=? l"an") (or (string=? l"the") (or (string=? l"those") (string=? l"these")))))) false]
  ))
(define (remove-articles l)
  (cond
    [(empty? l) empty]
    [else (cons (remove-articles-internal (first l)) (remove-articles (rest l)))]))
    
(define (remove-articles-internal l)
  (cond 
    [(empty? l) empty]
    [(article? (first l))  (remove-articles-internal (rest l))]
    [else (cons (first l) (remove-articles-internal (rest l)))]))
