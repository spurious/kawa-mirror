(module-export list-length-1 list-length-3 classify)

(define (list-length-1 x) :: <integer> (length x))
(define (list-length-2 x) :: <int> (list-length-1 x))
(define list-length-3 #t)
(set! list-length-3 list-length-2)

;; Caused VerifyError
(define name internal-node-name)
(define (classify)
  (let node-loop ((classes '()))
    (map (lambda (class)
	   (lambda (node1)
	     (name node1)))
	 classes)))
