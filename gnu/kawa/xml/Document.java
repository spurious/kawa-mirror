// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import java.net.URL;
import gnu.text.*;
import gnu.kawa.functions.BaseUri;

/** Implement the XQuery function 'document'. */

public class Document extends Procedure1
{
  public static final Document document = new Document();

  /** Resolve relative URI, and return an URL instance. */
  public static URL makeURL(Object url, Object base)
    throws java.net.MalformedURLException
  {
    if (url instanceof URL)
      return (URL) url;
    String name = url.toString();
    if (! BaseUri.hasScheme(name))
      {
	if (base != null)
	  {
	    Object b = BaseUri.baseUri(base);
	    name = BaseUri.resolve(name,
			   b == Values.empty ?  base.toString()
			   : b.toString());
	  }
	if (! BaseUri.hasScheme(name))
	  {
	    name = BaseUri.resolve(name, BaseUri.baseUri().toString());
	  }
      }
    return new URL(name);
  }

  public static URL makeURL(Object url)
    throws java.net.MalformedURLException
  {
    return makeURL(url, null);
  }

  public static void parse (Object name, Consumer out) throws Throwable
  {
    URL url = makeURL(name, null);
    SourceMessages messages = new SourceMessages();
    XMLParser parser = new XMLParser(url, messages, out);
    out.beginDocument();
    if (out instanceof TreeList)
      ((TreeList) out).writeBaseUri(url);
    parser.parse();
    if (messages.seenErrors())
      throw new SyntaxException("document function read invalid XML",
				messages);
    out.endDocument();
  }

  public static NodeTree parse (Object url) throws Throwable
  {
    NodeTree doc = new NodeTree();
    parse(url, doc);
    return doc;
  }

  public Object apply1 (Object arg1) throws Throwable
  {
    return parse(arg1.toString());
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object url = ctx.getNextArg();
    Object base = ctx.getNextArg(null);
    if (url instanceof Values)
      {
	int iter = 0;
	Values vals = (Values) url;
	for (;;)
	  {
	    iter = vals.nextPos(iter);
	    if (iter == 0)
	      break;
	    Object val = vals.getPosPrevious(iter);	
	    parse(makeURL(url, base), ctx.consumer);
	  }
      }
    else
      {
	parse(makeURL(url, base), ctx.consumer);
      }
  }
}
