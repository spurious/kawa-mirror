;;; Parse an XML file (specified by a URL or url string), giving a <document>.
(define (parse-xml-from-url url) :: <document>
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (invoke-static <gnu.xml.ParsedXMLToConsumer> 'parse url))

#|
(define (parse-nsxml-from-url url) :: <document>
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (let* ((doc :: <document> (make <document>))
	 (parser :: <gnu.xml.XMLParser>
		 (make <gnu.xml.XMLParser> url
		       (make <gnu.xml.ParsedXMLToConsumer>
			 (make <gnu.xml.NamespaceResolver> doc)))))
    (invoke parser 'parse)
    doc))

(define (print-nsxml-from-url url #!optional (out (current-output-port)))
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (let ((parser :: <gnu.xml.XMLParser>
		(make <gnu.xml.XMLParser> url
		      (make <gnu.xml.ParsedXMLToConsumer>
			(make <gnu.xml.NamespaceResolver>
			  (make <gnu.xml.XMLPrinter> out))))))
    (invoke parser 'parse)))
|#

;;; Print a Consumable (such as a <document>) to a port, in XML syntax.
(define (print-as-xml (data :: <gnu.lists.Consumable>)
		      #!optional (out (current-output-port)))
  (invoke data 'consume (make <gnu.xml.XMLPrinter> out)))
