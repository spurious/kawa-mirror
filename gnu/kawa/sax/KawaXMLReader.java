// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.sax;
import org.xml.sax.*;
import gnu.xml.*;
import gnu.kawa.xml.XMLParser;
import java.io.*;
import gnu.text.*;

/** An implementation of SAX2 XMLReader.
 */

public class KawaXMLReader extends ContentConsumer
implements XMLReader, Locator
{
  XMLParser xmlParser;

  public boolean getFeature (String name)
  {
    return false;
  }

  public void setFeature (String name, boolean value)
  {
  }

  public Object getProperty (String name)
  {
    return null;
  }

  public void setProperty (String name, Object value)
  {
  }

  public void setEntityResolver (EntityResolver resolver)
  {
  }

  public EntityResolver getEntityResolver ()
  {
    return null;
  }

  public void setDTDHandler (DTDHandler handler)
  {
  }

  public DTDHandler getDTDHandler ()
  {
    return null;
  }

  ErrorHandler errorHandler;


  public void setErrorHandler (ErrorHandler handler)
  {
    errorHandler = handler;
  }

  public ErrorHandler getErrorHandler ()
  {
    return errorHandler;
  }

  public void parse (InputSource input)
    throws IOException, SAXException
  {
    Reader reader = input.getCharacterStream();
    if (reader == null)
      reader = new InputStreamReader(input.getByteStream());
    SourceMessages messages = new SourceMessages();
    XMLParser parser = new XMLParser(new LineBufferedReader(reader),
				     messages, this);
    startDocument();
    parser.parse();
    String err = messages.toString(20);
    if (err != null)
      throw new SAXParseException(err, this);
    endDocument();
  }

  public void parse (String systemId)
  {
  }

  public String getPublicId()
  {
    return xmlParser.getPublicId();
  }

  public String getSystemId()
  {
    return xmlParser.getSystemId();
  }

  public int getColumnNumber()
  {
    int column = xmlParser.getColumnNumber();
    return column <= 0 ? -1 : column + 1;
  }

  public int getLineNumber()
  {
    int line = xmlParser.getLineNumber();
    return line <= 0 ? -1 : line + 1;
  }
}
