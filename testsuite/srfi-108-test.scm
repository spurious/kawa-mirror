(test-begin "srfi-108")

;; Some tests are based on Racket/Scribble documentation.

(cond-expand
 (kawa
  ;; Using 3-operand datum->syntax enables line numbers in reporting.
  (define-syntax xtest
    (lambda (form)
      (syntax-case form ()
        ;; We need to use the rest1 and rest2 variables since the Kawa reader
        ;; currently only attaches line-numbers to pairs, and the quoted and
        ;; evaluated sub-forms aren't guaranteed to be lists.
        ((strtest value . rest1)
         (syntax-case #'rest1 ()
           ((quoted . rest2)
            (syntax-case #'rest2 ()
              ((evaluated)
               #`(begin
             #,(datum->syntax form #'(test-equal quoted (quote value))
                              #'rest1)
             #,(datum->syntax form #'(test-equal evaluated (format "~a" value))
                              #'rest2)))))))))))
 (else
  (define-syntax xtest
    (syntax-rules ()
      ((xtest value quoted evaluated)
       (begin
         (test-equal quoted (quote value))
         (test-equal evaluated (format "~a" value))))))))

(define-syntax $construct$:doc
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<div>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ div) arg ...))))

(define-syntax $construct$:chapter
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<h1>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ h1) arg ...))))

(define-syntax $construct$:section
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<h2>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ h2) arg ...))))

(define-syntax $construct$:itemlist
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<ul>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ ul) arg ...))))

(define-syntax $construct$:item
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<li>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ li) arg ...))))

(define-syntax $construct$:emph
  (syntax-rules ()
    ((_ arg ...)
     ;; FIXME: #<em>&[arg ...]</>)))
     ($xml-element$ () ($resolve-qname$ em) arg ...))))

(xtest &emph{abc}
       '($construct$:emph "abc")
       &{<em>abc</em>})

(xtest &emph{abc&["n1" 3 "m2"]z}
       '($construct$:emph "abc" |$[$| "n1" 3 "m2" |$]$| "z")
       &{<em>abcn13m2z</em>})

(xtest &emph{abc&(+ 3 4)z}
       '($construct$:emph "abc" |$[$| (+ 3 4) |$]$| "z")
       &{<em>abc7z</em>})

(xtest &itemlist[&item{We have cookies for you.}
                 &item{If you want to eat a cookie,
                       you must bring your own straw.}]
       '($construct$:itemlist
         ($construct$:item "We have cookies for you.")
         ($construct$:item "If you want to eat a cookie,
                       you must bring your own straw.")
         |$]$|)
       "<ul><li>We have cookies for you.</li><li>If you want to eat a cookie,\n                       you must bring your own straw.</li></ul>")

(test-end)
