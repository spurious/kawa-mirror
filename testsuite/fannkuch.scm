;; fannkuch benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;;
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; Converted to Kawa by Per Bothner

(define (vector-reverse-slice! (v :: int[]) (i :: int) (j :: int)) :: void
  (let loop ((j :: int (- j 1))) ; exclude position j
    (if (< i j)
	(let ((t (v i)))
	  (set! (v i) (v j))
	  (set! (v j) t)
	  (set! i (+ i 1))
	  (loop (- j 1))))))

(define (count-flips (pi :: int[])) :: int
  (do ((rho :: int[] (vector-copy pi))
       (i :: int 0 (+ i 1)))
        ((= (rho 0) 0) i)
      (vector-reverse-slice! rho 0 (+ (rho 0) 1))))

(define (vector-copy (source :: int[])) :: int[]
  (let ((vec (int[] length: source:length)))
    (do ((i :: int 0 (+ i 1)))
	((= i source:length) vec)
      (set! (vec i) (source i)))))

(define (fannkuch n)
  (let ((pi (do ((pi (int[] length: n))
		 (i 0 (+ i 1)))
                ((= i n) pi)
	      (set! (pi i) i)))
	(r n)
	(count (int[] length: n)))
    (let loop ((flips 0)
	       (perms 0))
      (cond ((< perms 30)
	     (do ((i :: int 0 (+ i 1)))
		 ((>= i n))
	       (format #t "~d" (+ (pi i) 1)))
	     (newline)))
     ;; (format #t "n:~d r:~d~%~!" n r)
      (do ()
          ((= r 1))
	(set! (count (- r 1)) r)
	(set! r (- r 1)))
      (let ((flips2 (max (count-flips pi) flips)))
	(let ((result
	       (let loop2 ()
		 (if (= r n)
		     flips2
		     (let ((perm0 (pi 0)))
		       (do ((i 0))
			   ((>= i r))
			 (let ((j (+ i 1)))
			   (set! (pi i) (pi j))
			   (set! i j)))
		       (set! (pi r) perm0)
		       (set! (count r) (- (count r) 1))
		       (cond ((<= (count r) 0)
			      (set! r (+ r 1))
			      (loop2))
			     (else
			      #f)))))))
	  (or result
	      (loop flips2 (+ perms 1)))
	  )))))

(define args (cdr (command-line)))
(if (< (length args) 1)
    (begin (display "An argument is required") (newline) 2)
    (let ((n (string->number (car args))))
      (if (not (integer? n))
	  (format #t "An integer is required~%")
	  (format #t "Pfannkuchen(~S) = ~s~%" n (fannkuch n)))))
