// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.sax;
import gnu.lists.*;
import gnu.xml.*;
import org.xml.sax.*;
import gnu.mapping.Symbol;

/** Forward SAX1 or SAX2 events to a Consumer.
 */

public class ConsumeSAXHandler implements DocumentHandler, ContentHandler
{
  Consumer out;

  public void setDocumentLocator(Locator locator)
  {
    // FIXME
  }

  public ConsumeSAXHandler(Consumer out)
  {
    this.out = out;
  }

  public void startDocument()
  {
    out.beginDocument();
  }

  public void endDocument()
  {
    out.endDocument();
  }

  public void startElement (String namespaceURI, String localName,
			    String qName, Attributes atts)
        throws SAXException
  {
    out.beginGroup(Symbol.make(namespaceURI, localName));
    int numAttributes = atts.getLength();
    for (int i = 0;  i < numAttributes;  i++)
      {
	out.beginAttribute(Symbol.make(atts.getURI(i), atts.getLocalName(i)));
	out.writeChars(atts.getValue(i));
	out.endAttribute();
      }
  }

  public void endElement (String namespaceURI, String localName,
			  String qName)
    throws SAXException
  {
    out.endGroup();
  }

  public void startElement (String name, AttributeList atts)
    throws SAXException
  {
    name = name.intern();  // ???
    out.beginGroup(name);  // FIXME
    int attrLength = atts.getLength();
    for (int i = 0; i < attrLength; i++)
      {
	name = atts.getName(i);
	name = name.intern();  // ?
	String type = atts.getType(i);
	String value = atts.getValue(i);
	out.beginAttribute(name);  // FIXME
	out.writeChars(value);
	out.endAttribute();
      }
  }

  public void endElement (String name)
    throws SAXException
  {
    out.endGroup();
  }

  public void characters (char ch[], int start, int length)
    throws SAXException
  {
    out.write(ch, start, length);
  }

  public void ignorableWhitespace (char ch[], int start, int length)
    throws SAXException
  {
    // FIXME
    out.write(ch, start, length);
  }

  public void processingInstruction(String target, String data)
  {
    // FIXME
  }

  public void startPrefixMapping (String prefix, String uri)
  {
    // FIXME
  }

  public void endPrefixMapping (String prefix)
  {
    // FIXME
  }

  public void skippedEntity (String name)
  {
    // FIXME
  }
}
