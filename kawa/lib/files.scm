(define (file-exists? filename)
  ((primitive-virtual-method "java.io.File" "exists" "boolean" ())
   (->pathname filename)))

(define (file-directory? filename)
  ((primitive-virtual-method "java.io.File" "isDirectory" "boolean" ())
   (->pathname filename)))

(define (file-readable? filename)
  ((primitive-virtual-method "java.io.File" "canRead" "boolean" ())
   (->pathname filename)))

(define (file-writable? filename)
  ((primitive-virtual-method "java.io.File" "canWrite" "boolean" ())
   (->pathname filename)))

;(define (file-modification-time filename)
;  ((primitive-virtual-method "java.io.File" "lastModified" "long" ())
;   (->pathname filename)))

(define (delete-file filename)
  ((primitive-virtual-method "java.io.File" "delete" "boolean" ())
   (->pathname filename)))

(define (rename-file oldname newname)
  ((primitive-virtual-method "java.io.File" "renameTo" "boolean"
			     ("java.io.File"))
   (->pathname oldname) (->pathname newname)))


(define (copy-file from to)
  (let ((in (open-input-file from))
	(out (open-output-file to)))
    (do ((ch (read-char in) (read-char in)))
	((eof-object? ch)
	 (close-output-port out)
	 (close-input-port in)
	 #!void)
      (write-char ch out))))

(define (create-directory dirname)
  ((primitive-virtual-method "java.io.File" "mkdir" "boolean" ())
   (->pathname dirname)))

; Taken from MIT Scheme
(define (->pathname filename)
  ((primitive-constructor "java.io.File" ("String"))
    filename))
  
; From scsh
;(define (directory-files [dir [dotfiles?]]) ...)
