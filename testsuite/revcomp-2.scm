;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;; Contributed by Per Bothner
;; Based on Java version #1 contributed by Anthony Donnefort
;; slightly modified to read 82 bytes at a time by Razii

(define-constant NL :: int 10) ; #\newline as byte
(define-constant GT ::int 62)  ; #\> as byte
(define-constant cmp :: byte[]
  (let* ((c (byte[] length: 128))
	 (from "TAGCVHRMYKBDU")
	 (to "ATCGBDYKRMVHA")
	 (i :: int (string-length from)))
    (do ((i :: int 0 (+ i 1))) ((>= i 128))
      (set! (c i) i))
    (let loop ()
      (set! i (- i 1))
      (let ((f (char->integer (string-ref from i)))
	    (t (char->integer (string-ref to i))))
	(set! (c f) t)
	(set! (c (+ f 32)) t))
      (if (> i 0)
	  (loop)))
    c))

(define-simple-class ReversibleByteArray (java.io.ByteArrayOutputStream)
  class-name: ".RevByteArray"
  ((reverse-and-print) ::void ;; throws Exception FIXME
   (if (> count 0)
       (let ((begin ::int 0)
	     (end ::int (- count 1)))
	 (let loop ()
	     (let ((old (buf begin)))
	       (set! begin (+ begin 1))
	       (if (not (= old NL))
		   (loop))))
	 (let loop ()
	   (cond ((<= begin end)
		  (if (= (buf begin) NL)
		      (set! begin (+ begin 1)))
		  (if (= (buf end) NL)
		      (set! end (- end 1)))
		  (if (<= begin end)
		      (let ((tmp (buf begin)))
			(set! (buf begin) (cmp (buf end)))
			(set! begin (+ begin 1))
			(set! (buf end) (cmp tmp))
			(set! end (- end 1))))
		  (loop))))
	 (java.lang.System:out:write buf 0 count)))))

(let ((line (byte[] length: 82))
      (buf (ReversibleByteArray)))
  (let loop ()
    (let ((read (java.lang.System:in:read line)))
      (if (>= read 0)
	  (let ((last ::int 0))
	    (do ((i ::int 0 (+ i 1))) ((>= i read) #!void)
	      (cond ((= (line i) GT)
		     (buf:write line last (- i last))
		     (buf:reverse-and-print)
		     (buf:reset)
		     (set! last i))))
	    (buf:write line last (- read last))
	    (loop)))))
  (buf:reverse-and-print))

