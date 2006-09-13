(require 'http)
(unescaped-data "<?xml version='1.0'?>
")
(make-element 'body
	      (make-element 'p
			    "The request URI was: " (request-uri))
	      (make-element 'p
			    (let ((query (request-query-string)))
			      (if query
				  (values-append "The query string was: "
						 query)
				  "There was no query string."))))
