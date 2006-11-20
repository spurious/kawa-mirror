// Copyright (c) 2002. 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import java.io.*;
import gnu.mapping.*;
import java.util.Vector;

/** Output as an Http response.
 * Used for both CGI scripts (default) and HttpServletResponse (future).
 */

public class HttpPrinter extends FilterConsumer
{
  Vector headers = new Vector();
  /* #ifdef JAVA5 */
  // StringBuilder sbuf = new StringBuilder(100);
  /* #else */
  StringBuffer sbuf = new StringBuffer(100);
  /* #endif */
  Object currentHeader;

  /** 1 - implicit; 2: explicit. */
  private int seenBeginDocument;

  protected String sawContentType;

  /** Difference between number of beginGroup and endGroup calls so far. */
  private int groupNesting;

  protected OutputStream ostream;
  OutPort writer;

  public HttpPrinter(OutputStream out)
  {
    super(null);
    ostream = out;
    //ostream = System.out;
  }

  public HttpPrinter(OutPort out)
  {
    super(null);
    writer = out;
    //ostream = System.out;
  }

  public static HttpPrinter make (OutPort out)
  {
    return new HttpPrinter(out);
  }

  private void writeRaw(String str)
    throws java.io.IOException
  {
    if (writer != null)
      writer.write(str);
    else
      {
	int len = str.length();
	for (int i = 0;  i < len;  i++)
	  ostream.write((byte) str.charAt(i));
      }
  }

  public void printHeader(String label, String value)
    throws java.io.IOException
  {
    writeRaw(label);
    writeRaw(": ");
    writeRaw(value); // FIXME - need to quote?
    writeRaw("\n");
  }

  public void printHeaders()
    throws java.io.IOException
  {
    int num = headers.size();
    for (int i = 0;  i < num;  i += 2)
      printHeader(headers.elementAt(i).toString(),
		  headers.elementAt(i + 1).toString());
    //  if (sawContentType == null) writeRaw("Content-Type: text/html"); FIXME
    writeRaw("\n");
  }

  public void addHeader(String label, String value)
  {
    if (label.equalsIgnoreCase("Content-type"))
      sawContentType = value;
    headers.addElement(label);
    headers.addElement(value);
  }

  public void beginAttribute(Object attrType)
  {
    if (base == null)
      currentHeader = attrType;
    else
      base.beginAttribute(attrType);
  }

  public void endAttribute()
  {
    if (currentHeader != null)
      {
	addHeader(currentHeader.toString(), sbuf.toString());
	sbuf.setLength(0);
	currentHeader = null;
      }
    else
      base.endAttribute();
  }

  boolean seenXmlHeader;

  public void beginData()
  {
    if (base == null)
      {
	if (sawContentType == null)
	  addHeader("Content-type", "text/html");
	if (writer == null)
	  writer = new OutPort(ostream); // FIXME use encoding.
	String style = null;
	if ("text/html".equalsIgnoreCase(sawContentType))
	  style = "html";
	else if ("text/xhtml".equalsIgnoreCase(sawContentType))
	  style = "xhtml";
	else if ("text/plain".equalsIgnoreCase(sawContentType))
	  style = "plain";
	base = XMLPrinter.make(writer, style);
        if (seenBeginDocument == 0)
          {
            base.beginDocument();
            seenBeginDocument = 1;
          }
	try
	  {
	    printHeaders();
	  }
	catch (Throwable ex)
	  {
	    throw new RuntimeException(ex.toString());
	  }
      }
    /* #ifdef use:java.lang.CharSequence */
    append(sbuf);
    /* #else */
    // append(sbuf.toString());
    /* #endif */
    sbuf.setLength(0);
  }

  public void beginGroup(Object type)
  {
    if (sawContentType == null)
      {
	String mimeType;
	if (! seenXmlHeader)
	  mimeType = "text/html";
	else if (type instanceof Symbol
                 && "html".equals(((Symbol) type).getLocalPart()))
	  mimeType = "text/xhtml";
	else
	  mimeType = "text/xml";
	addHeader("Content-type", mimeType);
      }
    beginData();
    base.beginGroup(type);
    groupNesting++;
  }

  public void endGroup()
  {
    super.endGroup();
    groupNesting--;
    if (groupNesting == 0 && seenBeginDocument == 1)
      endDocument();
  }

  public void writeObject(Object v)
  {
    if (v instanceof Consumable && ! (v instanceof UnescapedData))
      ((Consumable) v).consume(this);
    else
      {
	beginData();
	super.writeObject(v);
      }
  }

  /* #ifdef use:java.lang.CharSequence */
  public Consumer append (CharSequence csq, int start, int end)
  {
    if (base == null)
      {
        /* #ifdef JAVA5 */
        // sbuf.append(csq, start, end);
        /* #else */
        if (csq == null)
          csq = "null";
        sbuf.append(csq.subSequence(start, end).toString());
        /* #endif */
      }
    else
      base.append(csq, start, end);
    return this;
  }

  public Consumer append (CharSequence csq)
  {
    if (base == null)
      {
        /* #ifdef JAVA5 */
        // sbuf.append(csq);
        /* #else */
        sbuf.append(csq.toString());
        /* #endif */
      }
    else
      base.append(csq);
    return this;
  }
  /* #else */
  // public Consumer append (String str)
  // {
  //   if (base == null)
  //     sbuf.append(str);
  //   else
  //     base.append(str);
  //   return this;
  // }
  /* #endif */

  public void write(char[] buf, int off, int len)
  {
    if (base == null)
      sbuf.append(buf, off, len);
    else
      base.write(buf, off, len);
  }

  public void beginDocument()
  {
    if (base != null)
      base.beginDocument();
    seenBeginDocument = 2;
  }

  public void endDocument()
  {
    if (base != null)
      base.endDocument();
    try
      {
	// else ???;
	if (writer != null)
	  writer.close();
	if (ostream != null)
	  ostream.flush();
      }
    catch (Throwable ex)
      {
      }
  }
}
