(module-name <kawa.lib.rnrs.programs>)
(module-export command-line exit)
(require <kawa.lib.prim_syntax>)

(define (command-line) :: list
  (let ((arg0 "kawa")) ;; FIXME
    (cons arg0 (gnu.lists.LList:makeList kawa.repl:commandLineArgArray 0))))

(define (exit #!optional (status :: <int> 0)) :: #!void
  (invoke-static <output-port> 'runCleanups)
  (invoke-static <java.lang.System> 'exit status))
