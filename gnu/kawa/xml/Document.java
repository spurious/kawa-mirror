// Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;
import java.net.URL;
import gnu.text.*;

/** Implement the XQuery function 'document'. */

public class Document
{
  public static final Document document = new Document();

  public static void parse (Object name, Consumer out) throws Throwable
  {
    SourceMessages messages = new SourceMessages();
    if (out instanceof XConsumer)
      ((XConsumer) out).beginEntity(name);
    gnu.xml.XMLParser.parse(name, messages, out);
    if (messages.seenErrors())
      throw new SyntaxException("document function read invalid XML",
				messages);
    if (out instanceof XConsumer)
      ((XConsumer) out).endEntity();
  }

  public static KDocument parse (Object uri) throws Throwable
  {
    NodeTree tree = new NodeTree();
    parse(uri, (Consumer) tree);
    return new KDocument(tree, TreeList.BEGIN_ENTITY_SIZE << 1);
  }

  /** Internal namespace used to mange cached documents. */
  static String docNamespace = "http://gnu.org/kawa/cached-documents";

  public static KDocument parseCached (Object uri)
    throws Throwable
  {
    Symbol sym = Symbol.make(docNamespace, uri.toString());
    Environment env = Environment.getCurrent();
    synchronized (sym)
      {
        NamedLocation loc = env.getLocation(sym, null, true);
        Object val = loc.get(null);
        if (val != null)
          return (KDocument) val;
        KDocument doc = parse(uri);
        loc.set(doc);
        return doc;
      }
  }
}
