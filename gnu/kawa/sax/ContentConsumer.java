// Copyright (c) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.sax;
import gnu.lists.*;
import gnu.xml.*;
import org.xml.sax.*;
import gnu.mapping.Symbol;
import gnu.text.Char;

/** Forward Consumer events to a SAX2 ContentHandler.
 */

public class ContentConsumer implements Consumer, Attributes
{
  ContentHandler out;
  /** Current nesting of elements. */
  int nesting = 0;
  String[] names = new String[15];
  /** Number of attributes if in begin-element content. */
  int numAttributes;
  String[] attributes = new String[20];  
  char[] chBuffer;
  StringBuffer strBuffer = new StringBuffer(200);
  /** 1 if in start-tag, 2 if in attribute value, 0 otherwise. */
  int inStartTag;

  public ContentConsumer ()
  {
  }

  public ContentConsumer (ContentHandler handler)
  {
    out = handler;
  }

  public void error(String method, SAXException ex)
  {
    throw new RuntimeException("caught "+ex+" in "+method);
  }

  public int getLength() { return numAttributes; }

  public String getQName(int i)
  { return i >= numAttributes ? null : attributes[4 * i]; }

  public String getURI(int i)
  { return i >= numAttributes ? null : attributes[4 * i + 1]; }

  public String getLocalName(int i)
  { return i >= numAttributes ? null : attributes[4 * i + 2]; }

  public String getValue(int i)
  { return i >= numAttributes ? null : attributes[4 * i + 3]; }

  public String getType(int i)
  { return i >= numAttributes ? null : "CDATA"; }

  public int getIndex (String uri, String localPart)
  {
    for (int i = numAttributes;  --i >= 0; )
      {
	if (uri.equals(attributes[4 * i + 1])
	    && localPart.equals(attributes[4 * i + 2]))
	  return i;
      }
    return -1;
  }

  public int getIndex (String qName)
  {
    for (int i = numAttributes;  --i >= 0; )
      {
	if (qName.equals(attributes[4 * i]))
	  return i;
      }
    return -1;
  }

  public String getType (String uri, String localPart)
  {
    int i = getIndex(uri, localPart);
    return i < 0 ? null : "CDATA";
  }

  public String getType(String qName)
  {
    int i = getIndex(qName);
    return i < 0 ? null : "CDATA";
  }

  public String getValue (String uri, String localPart)
  {
    int i = getIndex(uri, localPart);
    return i < 0 ? null : attributes[4 * i + 3];
  }

  public String getValue(String qName)
  {
    int i = getIndex(qName);
    return i < 0 ? null : attributes[4 * i + 3];
  }

  void endStartTag()
  {
    if (inStartTag != 1)
      return;
    int i = 3 * (nesting - 1);
    try
      {
	out.startElement(names[i], names[i+1], names[i+2], this);
      }
    catch (SAXException ex)
      {
	error("startElement", ex);
      }
    // Is this desirable, for the sake of GC?
    for (i = 4 * numAttributes;  --i >= 0; )
      attributes[i] = null;
    numAttributes = 0;
    inStartTag = 0;
  }

  public void beginGroup(String typeName, Object type)
  {
    if (inStartTag == 1)
      endStartTag();
    flushStrBuffer();
    int i = 3 * nesting;
    if (i >= names.length)
      {
	String[] tmp = new String[2 * i];
	System.arraycopy(names, 0, tmp, 0, i);
	names = tmp;
      }
    String namespaceURI, localName;
    if (type instanceof Symbol)
      {
	Symbol sym = (Symbol) type;
	namespaceURI = sym.getNamespaceURI();
	localName = sym.getLocalName();
      }
    else if (type instanceof XName)
      {
	XName sym = (XName) type;
	namespaceURI = sym.getNamespaceURI();
	localName = sym.getLocalName();
      }
    else
      {
	namespaceURI = "";
	localName = type.toString();
      }
    names[i] = namespaceURI;
    names[i+1] = localName;
    names[i+2] = typeName;
    numAttributes = 0;
    inStartTag = 1;
    nesting++;
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    int i = 4 * numAttributes;
    if (i >= attributes.length)
      {
	String[] tmp = new String[2 * i];
	System.arraycopy(attributes, 0, tmp, 0, i);
	attributes = tmp;
      }
    String namespaceURI = ((Symbol) attrType).getNamespaceURI();
    String localName = ((Symbol) attrType).getLocalName();
    attributes[i] = attrName;
    attributes[i+1] = namespaceURI;
    attributes[i+2] = localName;
    numAttributes++;
    inStartTag = 2;
  }

  public void endAttribute()
  {
    attributes[4 * numAttributes - 1] = strBuffer.toString();
    strBuffer.setLength(0);
    inStartTag = 1;
  }

  public void beginDocument()
  {
    try
      {
	out.startDocument();
      }
    catch (SAXException ex)
      {
	error("beginDocument", ex);
      }
  }

  public void endDocument()
  {
    try
      {
	out.endDocument();
      }
    catch (SAXException ex)
      {
	error("endDocument", ex);
      }
  }

  public void endGroup(String typeName)
  {
    endStartTag();
    flushStrBuffer();
    nesting--;
    int i = 3 * nesting;
    try
      {
	out.endElement(names[i], names[i+1], names[i+2]);
      }
    catch (SAXException ex)
      {
	error("endElement", ex);
      }
    names[i] = null;
    names[i+1] = null;
    names[i+2] = null;
  }

  void flushStrBuffer()
  {
    if (strBuffer.length() > 0)
      {
	if (chBuffer == null)
	  chBuffer = new char[200];
	try
	  {
	    int slen = strBuffer.length();
	    int start = 0;
	    for (;;)
	      {
		int len = slen - start;
		if (len <= 0)
		  break;
		if (len > chBuffer.length)
		  len = chBuffer.length;
		strBuffer.getChars(start, start + len, chBuffer, start);
		out.characters(chBuffer, 0, len);
		start += len;
	      }
	    strBuffer.setLength(0);
	  }
	catch (SAXException ex)
	  {
	    error("characters", ex);
	  }
      }
  }

  public void write(char[] buf, int off, int len)
  {
    if (inStartTag == 1)
      endStartTag();
    if (inStartTag == 2)
      strBuffer.append(buf, off, len);
    else
      {
	flushStrBuffer();
	try
	  {
	    out.characters(buf, off, len);
	  }
	catch (SAXException ex)
	  {
	    error("characters", ex);
	  }
      }
  }

  public void writeChar(int v)
  {
    if (inStartTag == 1)
      endStartTag();
    strBuffer.append((char) v);
  }

  public void writeChars(String v)
  {
    if (inStartTag == 1)
      endStartTag();
    strBuffer.append(v);
  }

  public void writeObject(Object v)
  {
    // Maybe prepend ' '?  FIXME
    if (v instanceof Consumable)
      ((Consumable) v).consume(this);
    else if (v instanceof SeqPosition)
      {
	SeqPosition pos = (SeqPosition) v;
	pos.sequence.consumeNext(pos.ipos, this);
      }
    else if (v instanceof Char)
      writeChar(((Char) v).intValue());
    else
      writeChars(v == null ? "(null)" : v.toString());
  }

  public void writeBoolean(boolean v)
  {
    if (inStartTag == 1)
      endStartTag();
    // Maybe prepend ' '?  FIXME
    strBuffer.append(v);
  }

  public void writeLong(long v)
  {
    if (inStartTag == 1)
      endStartTag();
    // Maybe prepend ' '?  FIXME
    strBuffer.append(v);
  }

  public void writeInt(int v)
  {
    if (inStartTag == 1)
      endStartTag();
    // Maybe prepend ' '?  FIXME
    strBuffer.append(v);
  }

  public void writeFloat(float v)
  {
    if (inStartTag == 1)
      endStartTag();
    // Maybe prepend ' '?  FIXME
    strBuffer.append(v);
  }

  public void writeDouble(double v)
  {
    if (inStartTag == 1)
      endStartTag();
    // Maybe prepend ' '?  FIXME
    strBuffer.append(v);
  }

  public void finalize()
  {
    flushStrBuffer();
  }

  public boolean ignoring ()
  {
    return false;
  }

  public void setContentHandler (ContentHandler handler)
  {
    out = handler;
  }

  public ContentHandler getContentHandler ()
  {
    return out;
  }
}
