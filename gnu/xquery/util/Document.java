// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;

/** Implement the XQuery function 'document'. */

public class Document extends Procedure1
{
  public static final Document document = new Document();

  public static TreeList document (String fileName) throws Throwable
  {
    int len = fileName.length();
    boolean seenProto = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = fileName.charAt(i);
	if (ch == ':')
	  {
	    seenProto = true;
	    break;
	  }
	if (Character.isLetter(ch))
	  break;
      }
    if (! seenProto)
      fileName = "file:" + fileName;
    
    java.net.URL url = new java.net.URL(fileName);
    TreeList doc = new TreeList();
    doc.beginDocument();
    XMLParser parser
      = new XMLParser(url,
		      new ParsedXMLToConsumer(new NamespaceResolver(doc)));
    parser.parse();
    doc.endDocument();
    return doc;
  }

  public Object apply1 (Object arg1) throws Throwable
  {
    return document(arg1.toString());
  }
}
