(require 'gui)

(define Yes-button
  (button label: "Yes"
	  oncommand: (lambda ()
		       (format #t "Yes button pressed!~%~!"))))

(frame title: "Kawa GUI Test"
       contents: (values Yes-button (panel width: 100 height: 100) Yes-button)
       menubar: (menubar
		 (menu label: "File"
		       (menuitem label: "New"
				  oncommand: (lambda ()
					       (format #t "New menuitem pressed!~%~!")))
		       (menuitem label: "Close"
				 oncommand:
				 (lambda ()
				   (format #t "Close menuitem pressed!~%~!")
				   (exit))))
		 (menu label: "Edit"
		       (menuitem label: "Cut"
				 oncommand: (lambda ()
					       (format #t "Cut menuitem pressed!~%~!"))))))
