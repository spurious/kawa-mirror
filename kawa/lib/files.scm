(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.reflection>)
(require <kawa.lib.syntax>)
(require <kawa.lib.ports>)

(define (path? path) :: <boolean>
  (instance? path <gnu.text.Path>))
(define (filepath? path) :: <boolean>
  (instance? path <gnu.text.FilePath>))
(define (URI? path) :: <boolean>
  (instance? path <gnu.text.URIPath>))
(define (absolute-path? (path :: path)) :: <boolean>
  (path:isAbsolute))
(define (path-scheme (p :: path))
  (let ((s (p:getScheme)))
    (if (eq? s #!null) #f s)))
(define (path-authority (p :: path))
  (let ((s (p:getAuthority)))
    (if (eq? s #!null) #f s)))
(define (path-user-info (p :: path))
  (let ((s (p:getUserInfo)))
    (if (eq? s #!null) #f s)))
(define (path-host (p :: path))
  (p:getHost))
(define (path-file (p :: path))
  (let ((s (p:getPath)))
    (if (eq? s #!null) #f s)))
(define (path-directory (p :: path))
  (let ((s (p:getDirectory)))
    (if (eq? s #!null) #f (s:toString))))
(define (path-parent (p :: path))
  (let ((s (p:getParent)))
    (if (eq? s #!null) #f (s:toString))))
(define (path-last (p :: path))
  (let ((s (p:getLast)))
    (if (eq? s #!null) #f s)))
(define (path-extension (p :: path))
  (let ((s (p:getExtension)))
    (if (eq? s #!null) #f s)))
(define (path-port (p :: path)) :: <int>
  (p:getPort))
(define (path-query (p :: path))
  (let ((s (p:getQuery)))
    (if (eq? s #!null) #f s)))
(define (path-fragment (p :: path))
  (let ((s (p:getFragment)))
    (if (eq? s #!null) #f s)))

#|
(resolve-path path) ;; resolves symlinks
(path->complete-path path [base-path])
(path->directory-path path)
(string->path string)
(path->string path)
(build-path base-path sub-path ...)
(expand-path)
(simplify-path)
|#

(define (file-exists? (file :: path)) :: <boolean>
  (file:exists))

(define (file-directory? (file :: path)) :: <boolean>
  (file:isDirectory))

(define (file-readable? (file :: filepath)) :: <boolean>
  ((file:toFile):canRead))

(define (file-writable? (file :: filepath)) :: <boolean>
  ((file:toFile):canWrite))

;(define (file-modification-time (filename :: path)) :: <long>
;  (filename:getLastModified))

(define (delete-file (file :: filepath)) :: <void>
  (if (not (file:delete))
      (primitive-throw (<java.io.IOException>:new
			((format #f "cannot delete ~a" file):toString)))))

(define (rename-file (oldname :: filepath) (newname :: filepath))
  ((oldname:toFile):renameTo (newname:toFile)))

(define (copy-file (from :: path) (to :: path)) :: <void>
  (let ((in (open-input-file from))
	(out (open-output-file to)))
    (do ((ch (read-char in) (read-char in)))
	((eof-object? ch)
	 (close-output-port out)
	 (close-input-port in)
	 #!void)
      (write-char ch out))))

(define (create-directory (dirname :: filepath))
  ((dirname:toFile):mkdir))

;; In Scsh and Gambit.
(define (directory-files (dir :: filepath))
  (let ((files ((java.io.File (dir:toFile)):list)))
     (if (eq? files #!null) #f
         (gnu.lists.LList:makeList files 0)))) 

;; (define (directory-for-each proc directory) ...)

; Taken from MIT Scheme
(define (->pathname filename) :: path
  (path filename))
  
(define (%file-separator)
  (invoke-static <java.lang.System> 'getProperty 'file.separator))

(define (system-tmpdir)
  (let ((name :: <java.lang.String> ; Java2 only
	 (invoke-static <java.lang.System> 'getProperty 'java.io.tmpdir)))
    (if (not (eq? name #!null))
	name
	(let ((sep (%file-separator)))
	  (if (equal? sep "\\") "C:\\temp" "/tmp")))))

; From scsh
;(define (directory-files [dir [dotfiles?]]) ...)

(define (resolve-uri (uri :: path) (base :: path)) :: path
  (base:resolve uri))

; From MzLib.  Scsh has (create-temp-file [prefix]).
(define (make-temporary-file #!optional (fmt :: <string> "kawa~d.tmp"))
  :: filepath
  (filepath (gnu.kawa.functions.FileUtils:createTempFile (fmt:toString))))
