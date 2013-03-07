;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname articles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/batch-io)
(define file-words (read-words/line "ttt.txt"))
(define (article? l)
(cond
  [(or (string=? l"a") (or (string=? l"an") (or (string=? l"the") (or (string=? l"those") (string=? l"these"))))) true]
  [(false? (or (string=? l"a") (or (string=? l"an") (or (string=? l"the") (or (string=? l"those") (string=? l"these")))))) false]
  ))
; List-of-lists-of-strings -> String
; Collapse a list where each element represents a line. An empty list
; is a blank line, and a non-empty list is a list of words. Separate
; the lines with a \n character.
(define (collapse-lines line-list)
  (cond [(empty? line-list) "\n"]
        [(empty? (rest line-list)) (string-append (collapse-words (first line-list)) "\n")]
        [else (string-append (collapse-words (first line-list))
                             "\n"
                             (collapse-lines (rest line-list)))]))
 
(check-expect (collapse-lines empty) "\n")
(check-expect (collapse-lines (list (list "A" "short") empty (list "Poem"))) "A short\n\nPoem\n")
 
; List-of-strings -> String
; Collapse a list of words into a single string with a space following
; each word.
(define (collapse-words word-list)
  (cond [(empty? word-list)  ""]
        [(empty? (rest word-list)) (first word-list)]
        [else (string-append (first word-list)
                             " "
                             (collapse-words (rest word-list)))]))
 
(check-expect (collapse-words empty) "")
(check-expect (collapse-words (list "A" "little" "list" "of" "words")) "A little list of words")
 
; String List-of-lists-of-strings -> String
; Write out a list of lists of words to a file with the given filename.
(define (write-words/line filename line-list)
  (write-file filename (collapse-lines line-list)))
 
(define ttt (read-words/line "ttt.txt"))

(define (remove-articles l)
  (cond
    [(empty? l) empty]
    [else (cons (remove-articles-internal (first l)) (remove-articles (rest l)))]))
    
(define (remove-articles-internal l)
  (cond 
    [(empty? l) empty]
    [(article? (first l))  (remove-articles-internal (rest l))]
    [else (cons (first l) (remove-articles-internal (rest l)))]))
(write-words/line "ttt2.txt" (remove-articles ttt))
