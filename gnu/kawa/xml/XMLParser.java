// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import gnu.text.*;
import java.net.*;

public class XMLParser extends XMLParserChar
                       /* #ifdef JAXP-1.3 */
                       // implements
                       // org.xml.sax.Locator
                       /* #endif */
{
  SourceMessages messages;

  public XMLParser(LineBufferedReader reader, SourceMessages messages, Consumer out)
  {
    this(reader, new ParsedXMLToConsumer(out), messages, out);
    
  }

  private XMLParser(LineBufferedReader reader, ParsedXMLToConsumer resolver,
		    SourceMessages messages, Consumer out)
  {
    super(null, 0, 0, resolver);
    in = reader;
    this.messages = messages;
    resolver.setParser(this);
  }

  public XMLParser(LineBufferedReader reader, Consumer out, SourceMessages messages)
  {
    super(null, 0, 0, new ParsedXMLToConsumer(out));
    in = reader;
    this.messages = messages;
  }

  private XMLParser(Object uri, Consumer out, SourceMessages messages,
		    ParsedXMLToConsumer resolver,
		    LineBufferedReader lreader)
    throws java.io.IOException
  {
    super(null, 0, 0, resolver);
    in = lreader;
    resolver.setParser(this);
    lreader.setName(uri);
    this.messages = messages;
  }

  public XMLParser(Object uri, SourceMessages messages, Consumer out)
    throws java.io.IOException
  {
    this(uri, out, messages, new ParsedXMLToConsumer(out),
	 new LineBufferedReader(new java.io.BufferedInputStream(URI_utils.getInputStream(uri))));
  }

  public int fill(char[] buffer,  int start, int pos)
  {
    LineBufferedReader reader = (LineBufferedReader) in;
    int saved = pos - start;
    try
      {
	if (saved > 0)
	  {
	    int skipped = reader.skip(start - reader.pos);
	    reader.mark(saved + 1);
	    skipped = reader.skip(saved);
	  }
	else
	  {
	    reader.skip(pos - reader.pos);
	  }
	int x = reader.read();
	if (x <= 0)
	  return -1;
	if (saved > 0)
	  {
	    reader.reset();
	    reader.skip(saved);
	  }
	else
	  reader.unread_quick();
      }
    catch (java.io.IOException ex)
      {
	throw new RuntimeException(ex.getMessage());
      }
    this.pos = reader.pos;
    this.buffer = reader.buffer;
    return reader.limit - reader.pos;
  }

  public void error(char severity, String message)
  {
    LineBufferedReader reader = (LineBufferedReader) in;
    try
      {
	reader.skip(pos - reader.pos);
      }
    catch (Exception ex)
      {
      }
    String filename = reader.getName();
    if (filename != null && filename.startsWith("file:"))
      filename = filename.substring(5);
    int line = reader.getLineNumber();
    int column = reader.getColumnNumber();
    messages.error(severity, filename, line + 1, column >= 0 ? column + 1 : 0,
		   message);
  }

  public String getPublicId ()
  {
    return null;
  }

  public String getSystemId ()
  {
    return ((LineBufferedReader) in).getName();
  }

  public int getLineNumber()
  {
    int line = ((LineBufferedReader) in).getLineNumber();
    return line < 0 ? -1 : line + 1;
  }

  public int getColumnNumber()
  {
    int col = ((LineBufferedReader) in).getColumnNumber();
    return col < 0 ? -1 : col + 1;
  }

  public void close ()
    throws java.io.IOException
  {
    in.close();
  }
}
