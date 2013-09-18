(module-name <kawa.lib.rnrs.programs>)
(module-export command-line exit)
(require <kawa.lib.prim_syntax>)

(define (command-line) :: list
  (let* ((rest
          (gnu.lists.LList:makeList
           gnu.expr.ApplicationMainSupport:commandLineArgArray 0))
         (command ::java.lang.String
                  (try-catch
                   (let ((raw (java.lang.System:getProperty
                               "sun.java.command")))
                     (if (eq? raw #!null) #!null
                         ;; Strip off the tail of the property value that
                         ;; duplicates the rest value.
                         (let* ((frest (format #f "~{ ~a~}" rest))
                                (rlen (raw:length))
                                (flen (frest:length))
                                (alen (- rlen flen)))
                           (cond ((= flen 0)
                                  raw)
                                 ;; Sanity check
                                 ((and (>= alen 0)
                                       ((raw:substring alen):equals frest))
                                  (raw:substring 0 alen))
                                 (else
                                  #!null)))))
                   (exp java.lang.Throwable #!null)))
         (arg0 (if (eq? command #!null) "kawa"
                   ("java ":concat command))))
    (cons arg0 rest)))

(define (exit #!optional (code 0)) :: #!void
  (invoke-static <output-port> 'runCleanups)
  (let ((status :: int
		(cond ((integer? code) code)
		      (code 0)
		      (else -1))))
    (invoke-static <java.lang.System> 'exit status)))

