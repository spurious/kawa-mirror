// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import java.net.URL;

/** Implement the XQuery function 'document'. */

public class Document extends Procedure1
{
  public static final Document document = new Document();

  /** If there is no protocol specified, pre-pend "file:" and return a URL. */

  public static URL makeURL(String fileName)
    throws java.net.MalformedURLException
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
	if (! Character.isLetter(ch))
	  break;
      }
    if (! seenProto)
      fileName = "file:" + fileName;
    return new URL(fileName);
  }

  public static TreeList document (Object url) throws Throwable
  {
    TreeList doc = new TreeList();
    document(url instanceof URL ? (URL) url : makeURL(url.toString()), doc);
    return doc;
  }

  public static TreeList document (String fileName) throws Throwable
  {
    TreeList doc = new TreeList();
    document(makeURL(fileName), doc);
    return doc;
  }

  public static void document (URL url, Consumer out) throws Throwable
  {
    out.beginDocument();
    XMLParser parser
      = new XMLParser(url,
		      new ParsedXMLToConsumer(new NamespaceResolver(out)));
    parser.parse();
    out.endDocument();
  }

  public Object apply1 (Object arg1) throws Throwable
  {
    return document(arg1.toString());
  }

  public void apply (CallContext ctx) throws Throwable
  {
    String fileName = ctx.getNextArg().toString();
    document(makeURL(fileName), ctx.consumer);
  }
}
