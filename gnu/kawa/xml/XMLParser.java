// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import gnu.text.*;
import java.net.*;

public class XMLParser extends XMLParserChar
{
  SourceMessages messages;

  public XMLParser(LineBufferedReader reader, Consumer out, SourceMessages messages)
    throws java.io.IOException
  {
    super(reader, new ParsedXMLToConsumer(out));

    this.messages = messages;
  }

  public XMLParser(URL url, Consumer out, SourceMessages messages)
    throws java.io.IOException
  {
    super(url, new ParsedXMLToConsumer(out));

    this.messages = messages;
  }
}
