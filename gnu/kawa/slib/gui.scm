(define (make-action-listener proc)
  :: <java.awt.event.ActionListener>
  (if (instance? proc <java.awt.event.ActionListener>)
      proc
      (object (<java.awt.event.ActionListener>)
	      ((actionPerformed (e :: <java.awt.event.ActionEvent>)) :: <void>
	       (proc)))))

(define (button #!key
		(label :: <String> #!null)
		(image #!null)
		(default #!null)
		(oncommand #!null)
		(disabled #f)
		(accesskey #!null))
  :: <javax.swing.JButton>
  (let ((button :: <javax.swing.JButton>
		(make  <javax.swing.JButton>)))
    (if disabled
	(invoke button 'setEnabled #f))
    (if (not (eq? label #!null))
	(invoke button 'setText label))
    (if (not (eq? oncommand #!null))
	(invoke button 'addActionListener (make-action-listener oncommand)))
    button))

(define (frame #!key
	       (title  :: <String> #!null)
	       (menubar ::  <javax.swing.JMenuBar> #!null)
	       contents)
  :: <javax.swing.JFrame>
  (let ((frame :: <javax.swing.JFrame>
	       (make  <javax.swing.JFrame>)))
    (if (not (eq? title #!null))
	(invoke frame 'setTitle title))
    (if (not (eq? menubar #!null))
	(invoke frame 'setJMenuBar menubar))
    (invoke (invoke frame 'getContentPane) 'add contents)
    (invoke frame 'pack)
    (invoke frame 'show)
    frame))

(define (menubar #!rest args  :: <object[]>)
    :: <javax.swing.JMenuBar>
    (let ((menubar :: <javax.swing.JMenuBar>
		    (make  <javax.swing.JMenuBar>))
	  (num-args :: <int> (field args 'length)))
      (let loop ((i :: <int> 0))
	(if (< i num-args)
	    (let ((arg ((primitive-array-get <object>) args i)))
	      
	      (invoke menubar 'add (as <javax.swing.JMenu> arg))
	      (loop (+ i 1)))))
      menubar))

(define (menu #!rest args :: <object[]>)
    :: <javax.swing.JMenu>
    (let ((menu :: <javax.swing.JMenu>
		    (make  <javax.swing.JMenu>))
	  (num-args :: <int> (field args 'length)))
      (let loop ((i :: <int> 0))
	(if (< i num-args)
	    (let ((arg ((primitive-array-get <object>) args i)))
	      (cond ((and (eq? arg label:) (< (+ i 1) num-args))
		     (invoke menu 'setText
			     (as <String>
				 ((primitive-array-get <object>) args (+ i 1))))
		     (loop (+ i 2)))
		    (else
		     (invoke menu 'add (as <javax.swing.JMenuItem> arg))
		     (loop (+ i 1)))))))
      menu))

(define (menuitem #!key
		(label :: <String> #!null)
		(image #!null)
		(default #!null)
		(oncommand #!null)
		(disabled #f)
		(accesskey #!null))
  :: <javax.swing.JMenuItem>
  (let ((menuitem :: <javax.swing.JMenuItem>
		(make  <javax.swing.JMenuItem>)))
    (if disabled
	(invoke menuitem 'setEnabled #f))
    (if (not (eq? label #!null))
	(invoke menuitem 'setText label))
    (if (not (eq? oncommand #!null))
	(invoke menuitem 'addActionListener (make-action-listener oncommand)))
    menuitem))
