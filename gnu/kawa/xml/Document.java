// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import java.net.URL;
import gnu.text.*;

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

  public static void parse (Object name, Consumer out) throws Throwable
  {
    URL url = name instanceof URL ? (URL) name : makeURL(name.toString());
    SourceMessages messages = new SourceMessages();
    XMLParser parser
      = new XMLParser(url,
		      new NamespaceResolver(out),
		      messages);
    out.beginDocument();
    if (out instanceof TreeList)
      ((TreeList) out).writeBaseUri(url);
    parser.parse();
    if (messages.seenErrors())
      throw new SyntaxException(messages);
    out.endDocument();
  }

  public static TreeList parse (Object url) throws Throwable
  {
    TreeList doc = new TreeList();
    parse(url, doc);
    return doc;
  }

  public Object apply1 (Object arg1) throws Throwable
  {
    return parse(arg1.toString());
  }

  public void apply (CallContext ctx) throws Throwable
  {
    String fileName = ctx.getNextArg().toString();
    parse(makeURL(fileName), ctx.consumer);
  }
}
