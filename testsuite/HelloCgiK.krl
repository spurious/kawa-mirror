[(response-content-type "text/xml")
]<p>The request URL was: [(request-url)]</p>
<p>[(let ((query (request-query-string)))
    (if query
      ]There was no query string.[
      (begin ]The query string was: [query)))]</p>
