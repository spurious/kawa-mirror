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
  ((primitive-virtual-method <java.io.File> "mkdir" <boolean> ())
   (->pathname dirname)))

; Taken from MIT Scheme
(define (->pathname filename)
  ((primitive-constructor "java.io.File" ("String"))
    filename))
  
(define (%file-separator)
  (symbol->string
   (invoke-static <java.lang.System> 'getProperty 'file.separator)))

(define (system-tmpdir)
  (let ((name ; Java2 only
	 (invoke-static <java.lang.System> 'getProperty 'java.io.tmpdir)))
    (if (not (eq? name #!null))
	(symbol->string name)
	(let ((sep (%file-separator)))
	  (if (equal? sep "\\") "C:\\temp" "/tmp")))))

; From scsh
;(define (directory-files [dir [dotfiles?]]) ...)

(define-private *temp-file-number* 1)
; From MzLib.  Scsh has (create-temp-file [prefix]).
; This is not safe from race conditions!
; Fix this using new Java2 File.createTempFile.  FIXME.
; Should not be using /tmp.  FIXME.
(define (make-temporary-file #!optional (fmt "kawa~d.tmp"))
  (let loop ()
    (let ((fname (string-append (system-tmpdir) (%file-separator)
				(format #f fmt *temp-file-number*))))
      (set! *temp-file-number* (+ *temp-file-number* 1))
      (if (file-exists? fname)
	  (loop)
	  fname))))
