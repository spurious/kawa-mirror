(frame title: "Kawa GUI Test"
       contents: (button label: "Yes"
			 oncommand: (lambda ()
				      (format #t "Yes button pressed!~%~!")))
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
