(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.reflection>)
(require <kawa.lib.syntax>)
(require <kawa.lib.ports>)

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
  (make <string>
   (invoke-static <java.lang.System> 'getProperty 'file.separator)))

(define (system-tmpdir)
  (let ((name :: <java.lang.String> ; Java2 only
	 (invoke-static <java.lang.System> 'getProperty 'java.io.tmpdir)))
    (if (not (eq? name #!null))
	(make <string> name)
	(let ((sep (%file-separator)))
	  (if (equal? sep "\\") "C:\\temp" "/tmp")))))

; From scsh
;(define (directory-files [dir [dotfiles?]]) ...)

(define (URI url) :: <URI>
  (gnu.text.URI_utils:toURI url))

(define (resolve-uri uri base) :: <URI>
  (gnu.text.URI_utils:resolve uri base))

(define-syntax module-uri
  (lambda (form)
    (syntax-case form ()
      ((_)
       (gnu.kawa.functions.GetModuleClass:getModuleClassURI
	(gnu.expr.Compilation:getCurrent))))))

(define-syntax resource-uri
  (syntax-rules ()
    ((resource-uri uri)
     (gnu.text.URI_utils:resolve uri (module-uri)))))

; From MzLib.  Scsh has (create-temp-file [prefix]).
(define (make-temporary-file #!optional (fmt "kawa~d.tmp")) :: <string>
  (make <string> (gnu.kawa.functions.FileUtils:createTempFile fmt)))

;;; The definition of include is based on that in the portable implementation
;;; of syntax-case psyntax.ss, which is again based on Chez Scheme.
;;; Copyright (c) 1992-2002 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.
(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ((p (open-input-file fn)))
          (let f ()
            (let ((x (read p)))
              (if (eof-object? x)
                  (begin (close-input-port p) '())
                  (<pair> (datum->syntax-object k x) (f))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax-object->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

(define-syntax (include-relative x)
  (syntax-case x ()
	       ((_ filename)
		(let ((path-pair :: <pair>
				 (syntax-object->datum (syntax (filename)))))
		  (list
		   (datum->syntax-object (syntax filename) 'include)
		   (datum->syntax-object
		    (syntax filename)
		    (make <string>
		     (gnu.text.URI_utils:resolve
		      (field path-pair 'car)
		      (gnu.lists.PairWithPosition:getFile path-pair)))))))))

#|
(define-syntax source-file
  (lambda (x)
    (syntax-case x ()
		 ((_ form)
		  (let ((form (syntax-object->datum (syntax (form)))))
		    (if (instance? form <gnu.lists.PairWithPosition>)
			(list (quote quote)
			      (datum->syntax-object form (gnu.lists.PairWithPosition:getFile form)))
			#f))))))
|#
