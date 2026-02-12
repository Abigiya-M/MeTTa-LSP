; Test MeTTa file with various language features

; Function definition
(defun factorial (n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

; Let binding
(let ((x 10) (y 20))
  (+ x y))

; Pattern matching
(match ()
  ((a ) (b))
  (_ 0))

; Higher-order functions
(defun map-fn (f lst)
  (if (null? lst)
    ()
    (cons)
(let* ((a 5))
  a) (f (car lst)) (map-fn f (cdr lst))))

; Variable usage
$my-var

; String and number literals
"Hello, MeTTa!"
42
3.14
-7
.5

; Comments are preserved
; This is a line comment
(+ 1 2) ; inline comment

; Unmatched paren for error detection
(defun broken (x)
  (+ x 1) x
    
(: type Person)
(Person Alice)
(Person Bob)

(= (prent Alice Bob) True)

(= (ancestor ?x ?y)
   (and (parent ?x ?z)
        (ancestor ?z ?y)))

(acestor Alice Bb)
(anc)





