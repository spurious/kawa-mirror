(module-name <kawa.lib.rnrs.programs>)
(module-export command-line exit)
(require <kawa.lib.prim_syntax>)

(define (command-line) :: list
  (let ((arg0 "kawa")) ;; FIXME
    (cons arg0 (gnu.lists.LList:makeList gnu.expr.ApplicationMainSupport:commandLineArgArray 0))))

(define (exit #!optional (code 0)) :: #!void
  (invoke-static <output-port> 'runCleanups)
  (let ((status :: int
		(cond ((integer? code) code)
		      (code 0)
		      (else -1))))
    (invoke-static <java.lang.System> 'exit status)))

