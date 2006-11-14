// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import gnu.text.*;
import java.net.*;

public class XMLParser extends XMLParserChar
                       implements gnu.text.SourceLocator
{
  XMLFilter filter;

  public XMLParser(LineBufferedReader reader, SourceMessages messages, Consumer out)
  {
    this(reader, new XMLFilter(out), messages);
    
  }

  public XMLParser(Object uri, SourceMessages messages, Consumer out)
    throws java.io.IOException
  {
    this(uri, messages, new XMLFilter(out),
	 new LineBufferedReader(new java.io.BufferedInputStream(URI_utils.getInputStream(uri))));
  }

  private XMLParser(LineBufferedReader reader, XMLFilter resolver,
		    SourceMessages messages)
  {
    super(null, 0, 0, resolver);
    this.filter = resolver;
    in = reader;
    resolver.setMessages(messages);
    resolver.setSourceLocator(this);
  }

  private XMLParser(Object uri, SourceMessages messages,
		    XMLFilter resolver,
		    LineBufferedReader lreader)
    throws java.io.IOException
  {
    this(lreader, resolver, messages);
    lreader.setName(uri);
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
    filter.error(severity, message);
  }

  public String getPublicId ()
  {
    return null;
  }

  public String getSystemId ()
  {
    return ((LineBufferedReader) in).getName();
  }

  public String getFileName ()
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
    return col < 0 ? -1 : col;
  }

  public boolean isStableSourceLocation()
  {
    return false;
  }

  public void close ()
    throws java.io.IOException
  {
    in.close();
  }
}
