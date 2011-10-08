(test-begin "srfi-38" 8)

(define circ1 (list 1 2 3))
(define circ2 (list 1 2 (list 'a 'b 'c) (vector "gsoc" "kawa") 3))
(define circ3 (cons 'a 'b))
(define a (cons 'a 'z))
(define b (cons 'b a))
(define c (cons 'c b))
(define circ4 (list c b a))
(define (long-line n) (if (= n 0) '() (cons 'a (long-line (- n 1)))))
(define edge1 (long-line 500))

(define *list* '(b c d e f g h i j k l m n o p q u r s t v w x y))
(define (long-shared res n)
  (if (= n 24)
      res
      (list (long-shared (cons (list-ref *list* (- n 1)) res)
                         (+ n 1))
            res)))
(define circ5 (long-shared (cons 'a 'z) 10))

(define (make-write-string form)
  (call-with-output-string
   (lambda (s) (write form s))))

(set-car! (cdr circ1) circ1)
(test-equal "#1=(1 #1# 3)"
	    (make-write-string circ1))

(set-car! (cdr circ2) circ2)
(test-equal "#1=(1 #1# (a b c) #(\"gsoc\" \"kawa\") 3)"
	    (make-write-string circ2))

(set-car! (cddr (caddr circ2)) (caddr circ2))
(test-equal "#1=(1 #1# #2=(a b #2#) #(\"gsoc\" \"kawa\") 3)"
	    (make-write-string circ2))

(vector-set! (cadddr circ2) 1 (cadddr circ2))
(test-equal "#1=(1 #1# #2=(a b #2#) #3=#(\"gsoc\" #3#) 3)"
	    (make-write-string circ2))

(set-car! (cdr circ2) 2)
(test-equal "(1 2 #1=(a b #1#) #2=#(\"gsoc\" #2#) 3)"
	    (make-write-string circ2))

(set-cdr! circ3 circ3)
(test-equal "#1=(a . #1#)"
	    (make-write-string circ3))

(test-equal "((c . #1=(b . #2=(a . z))) #1# #2#)"
	    (make-write-string circ4))

;; Stress test.
(make-write-string edge1)

;; Testing both position markers > 9 and the hash table.
(test-equal
 "(((((((((((((((x . #1=(w . #2=(v . #3=(t . #4=(s . #5=(r . #6=(u . #7=(q . #8=(p . #9=(o . #10=(n . #11=(m . #12=(l . #13=(k . #14=(a . z))))))))))))))) #1#) #2#) #3#) #4#) #5#) #6#) #7#) #8#) #9#) #10#) #11#) #12#) #13#) #14#)"
  (make-write-string circ5))

(test-end)
