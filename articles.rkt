;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname articles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)
(define file-words (read-words "ttt.txt"))
(define-struct file (list counter))
(define (remove-articles l)
  (remove-articles-internal (make-file l 0)))
(define (remove-articles-internal l)
  (cond 
    [(empty?(first (file-list l))) empty]
    [ (or (string=? (first(file-list l))"a") (or (string=? (first(file-list l))"an") (or (string=? (first(file-list l))"the") (or (string=? (first(file-list l))"those") (string=? (first(file-list l))"these")))))  (remove-articles-internal (make-file (rest(file-list l)) (+ (file-counter l) 1)))]
    [else (cons (first(file-list l)) (remove-articles-internal (rest(file-list l))))]))