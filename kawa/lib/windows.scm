(define (scheme-window #!optional share)
  (let* ((interp :: <gnu.expr.Interpreter>
		 (invoke-static <gnu.expr.Interpreter> 'getInterpreter))
	 (env :: <gnu.mapping.Environment>
	      (if share (interaction-environment)
		  (invoke interp 'getNewEnvironment))))
    ((primitive-constructor <kawa.GuiConsole> (<gnu.expr.Interpreter>
					       <gnu.mapping.Environment>))
     interp env)))
