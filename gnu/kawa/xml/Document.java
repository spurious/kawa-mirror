// Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import java.net.URL;
import gnu.text.*;
import gnu.kawa.functions.BaseUri;

/** Implement the XQuery function 'document'. */

public class Document
{
  public static final Document document = new Document();

  public static void parse (Object name, Consumer out) throws Throwable
  {
    SourceMessages messages = new SourceMessages();
    XMLParser parser = new XMLParser(name, messages, out);
    out.beginDocument();
    if (out instanceof TreeList)
      ((TreeList) out).writeBaseUri(name);
    parser.parse();
    if (messages.seenErrors())
      throw new SyntaxException("document function read invalid XML",
				messages);
    out.endDocument();
  }

  public static KDocument parse (Object uri) throws Throwable
  {
    NodeTree doc = new NodeTree();
    parse(uri, doc);
    return new KDocument(doc, 0);
  }

  /** Internal namespace used to mange cached documents. */
  static String docNamespace = "http://gnu.org/kawa/cached-documents";

  public static Object parseCached (Object uri)
    throws Throwable
  {
    Symbol sym = Symbol.make(docNamespace, uri.toString());
    Environment env = Environment.getCurrent();
    synchronized (sym)
      {
        NamedLocation loc = env.getLocation(sym, null, true);
        Object val = loc.get(null);
        if (val != null)
          return val;

        NodeTree tree = new NodeTree();
        SourceMessages messages = new SourceMessages();
        XMLParser parser = new XMLParser(uri, messages, tree);
        tree.beginDocument();
        tree.writeBaseUri(uri);
        parser.parse();
        parser.close();
        if (messages.seenErrors())
          throw new SyntaxException("document function read invalid XML",
                                    messages);
        tree.endDocument();
        val = new KDocument(tree, 0);
        loc.set(val);
        return val;
      }
  }

  /** Parse an XML document, caching the result.
   * Only positive results are cached; failures are not.)
   * This implements the standard XQuery <code>fn:doc</code> function.
   */
  public static Object parseCached (Object uri, Object base)
    throws Throwable
  {
    if (uri == Values.empty || uri == null)
      return uri;
    if (! (uri instanceof URL))
      {
        String name = StringValue.stringValue(uri);
        if (! InPort.uriSchemeSpecified(name))
          {
            if (base == null)
              base = BaseUri.baseUri();
            uri = URI_utils.resolve(uri, base);
          }
      }
    return parseCached(uri);
  }

  /** Check if an XML document is available, caching the result.
   * Only positive results are cached; failures are not.  Thus it is possible
   * for a false result to be followed by a true result, but not vice versa.
   * This implements the standard XQuery <code>fn:doc-available</code> function.
   */
  public static boolean availableCached (Object url, Object base)
  {
    if (url == Values.empty || url == null)
      return false;
   try
      {
        parseCached(url, base);
        return true;
      }
    catch (Throwable ex)
      {
        return false;
      }
  }
}
