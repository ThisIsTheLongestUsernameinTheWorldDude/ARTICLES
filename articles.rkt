;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname articles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)
(define file-words (read-words "ttt.txt"))
(define-struct file (list counter))
(define (remove-articles l)
  (remove-articles-internal (make-file l 0)))
(define (article? l)
(cond
  [(or (string=? l"a") (or (string=? l"an") (or (string=? l"the") (or (string=? l"those") (string=? l"these"))))) true]
  [(false? (or (string=? l"a") (or (string=? l"an") (or (string=? l"the") (or (string=? l"those") (string=? l"these")))))) false]
  ))
(define (remove-articles-internal l)
  (cond
  [(file? l)(cond 
    [(empty?(first (file-list l))) empty]
    [(article? (first(file-list l)))  (remove-articles-internal (make-file (rest(file-list l)) (+ (file-counter l) 1)))]
    [else (make-file (cons (first(file-list l)) (remove-articles-internal (rest(file-list l)))) (file-counter l))])]
  [(cons? l) "wtf"]
  [else "wat"]))          