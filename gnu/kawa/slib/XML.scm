;;; Parse an XML file (specified by a URL or url string), giving a <document>.
(define (parse-xml-from-url url) :: <document>
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (invoke-static <gnu.kawa.xml.ParsedXMLToConsumer> 'parse url))

;;; Print a Consumable (such as a <document>) to a port, in XML syntax.
(define (print-as-xml (data :: <gnu.lists.Consumable>)
		      #!optional (out (current-output-port)))
  (invoke data 'consume (make <gnu.kawa.xml.XMLPrinter> out)))
