(test-init "macros" 41)

(test 'ok 'letxx (let ((xx #f)) (cond (#t xx 'ok))))

;;; FIXME this does not work - hygiene problem!
;;;(test 'ok 'let=> (let ((=> #f)) (cond (#t => 'ok))))

(load (string-append src-prefix "mac1.scm"))
(test '(1 2) 'something (something 1 2))

(test '(2 3) 'something (something 2 3))

;;; From Common Lisp the Language 2nd ed page 198
(defmacro arithmetic-if (test neg-form zero-form pos-form)
  (let ((var (gentemp)))
    `(let ((,var ,test))
       (cond ((< , var 0) ,neg-form)
             ((= ,var 0) ,zero-form)
             (#t ,pos-form)))))

(test "POS" 'arithmetic-if-pos (arithmetic-if 234 "NEG" "ZERO" "POS"))
(test "NEG" 'arithmetic-if-pos (arithmetic-if -234 "NEG" "ZERO" "POS"))

;;; Posted to comp.lang.scheme by mooreb@lark.cc.ukans.edu (Brian M. Moore)
(test '(x) 'lambda*3
      ((lambda lambda lambda) 'x))
(test '(1 2 3) 'lambda-begin
      ((lambda (begin) (begin 1 2 3)) (lambda lambda lambda)))

;;; From R5RS:
(test 'now 'let-syntax-1
      (let-syntax
          ((when (syntax-rules ()
                               ((when test stmt1 stmt2 ...)
                                (if test
                                    (begin stmt1 stmt2 ...))))))
        (let ((if #t))
          (when if (set! if 'now))
          if)))

;;; From R5RS:
(test 'outer 'let-syntax-2
      (let ((x 'outer))
        (let-syntax ((m (syntax-rules () ((m) x))))
          (let ((x 'inner))
            (m)))))                ;       =>  outer


;;; Based on an example Listed as an "error" in R5RS.
;;; (We don't actually complain about the erroneous version.)
(test 6 'let-syntax-3
      (let-syntax
          ((foo (syntax-rules ()
                              ((foo (proc args ...) body ...)
                               (define proc
                                 (lambda (args ...)
                                   body ...))))))
        (let ((x 3))
          (foo (plus x y) (+ x y))
          (let () ;; Added this extra let to make it legit.
            (define foo x)
            (plus foo x)))))

;;; From R5RS:
(test 7 'letrec-syntax-1
      (letrec-syntax
       ((my-or (syntax-rules ()
                             ((my-or) #f)
                             ((my-or e) e)
                             ((my-or e1 e2 ...)
                              (let ((temp e1))
                                (if temp
                                    temp
                                    (my-or e2 ...)))))))
       (let ((x #f)
             (y 7)
             (temp 8)
             (let odd?)
             (if even?))
         (my-or x
                (let temp)
                (if y)
                y))))

(define (internal-define-syntax)
  (let ()
    (define-syntax ten (syntax-rules () ((ten) 10)))
    (define x (ten))
    x))
(test 10 internal-define-syntax)

;; Based on bug report from Stephen L. Peters <portnoy@portnoy.org>:
(define-syntax test-ds1 (syntax-rules () ((test-ds1 x) (list 'x))))
(test '((t1)) 'test-ds1 (test-ds1 (t1)))
(test '((t2)) 'test-ds2
      (begin
	(define-syntax test-ds2 (syntax-rules () ((test-ds2 x) (list 'x))))
	(test-ds2 (t2))))

(test '() 'cut-1 ((cut list)))
(test '() 'cut-2  ((cut list <...>)))
(test '(1) 'cut-3 ((cut list 1)))
(test '(1) 'cut-4 ((cut list <>) 1))
(test '(1) 'cut-5 ((cut list <...>) 1))
(test '(1 2) 'cut-6 ((cut list 1 2)))
(test '(1 2) 'cut-7 ((cut list 1 <>) 2))
(test '(1 2) 'cut-8 ((cut list 1 <...>) 2))
(test '(1 2 3 4) 'cut-9 ((cut list 1 <...>) 2 3 4))
(test '(1 2 3 4) 'cut-10 ((cut list 1 <> 3 <>) 2 4))
(test '(1 2 3 4 5 6) 'cut-11 ((cut list 1 <> 3 <...>) 2 4 5 6))
(test '(ok) 'cut-12 (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)))
(test 2 'cut-13
      (let ((a 0))
	(map (cut + (begin (set! a (+ a 1)) a) <>)
	     '(1 2))
	a))

;; cutes
(test '() 'cute-1 ((cute list)))
(test '() 'cute-2 ((cute list <...>)))
(test '(1) 'cute-3 ((cute list 1)))
(test '(1) 'cute-4 ((cute list <>) 1))
(test '(1) 'cute-5 ((cute list <...>) 1))
(test '(1 2) 'cute-6 ((cute list 1 2)))
(test '(1 2) 'cute-7 ((cute list 1 <>) 2))
(test '(1 2) 'cute-8 ((cute list 1 <...>) 2))
(test '(1 2 3 4) 'cute-9 ((cute list 1 <...>) 2 3 4))
(test '(1 2 3 4) 'cute-10 ((cute list 1 <> 3 <>) 2 4))
(test '(1 2 3 4 5 6) 'cute-11 ((cute list 1 <> 3 <...>) 2 4 5 6))
(test 1 'cute-12
      (let ((a 0))
	(map (cute + (begin (set! a (+ a 1)) a) <>)
	     '(1 2))
	a))

(define-syntax test-set
 (syntax-rules ()
   ((test-set)
    (let ((s 1))
      (set! s (+ s 1))
      s))))
(test 2 'test-set (test-set))

(define-syntax test-colon
 (syntax-rules ()
   ((test-colon x)
    (let loop ((size :: <int> 10))
      (+ size x)))))
(test 14 'test-colon (test-colon 4))
