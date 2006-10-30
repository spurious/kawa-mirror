// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.lists.*;
import java.util.Stack;

public class NodeUtils
{
  public static String name (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
    Object name = NodeName.nodeName(node);
    if (name == null || name == Values.empty)
      return "";
    return name.toString();
  }

  public static String localName (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
    if (! (node instanceof KNode))
      throw new WrongType("local-name", 1, node, "node()?");
    Object name = NodeName.nodeName(node);
    if (name == null || name == Values.empty)
      return "";
    return ((Symbol) name).getName();
  }

  public static Object namespaceURI (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
    if (! (node instanceof KNode))
      throw new WrongType("namespace-uri", 1, node, "node()?");
    Object name = NodeName.nodeName(node);
    if (name == null || name == Values.empty)
      return "";
    return QNameUtils.namespaceURIFromQName(name);
  }

  public static void prefixesFromNodetype (XName name, Consumer out)
  {
    NamespaceBinding bindings = ((XName) name).getNamespaceNodes();
    for (NamespaceBinding ns = bindings;
         ns != null;
         ns = ns.getNext())
      {
        String prefix = ns.getPrefix();
        // Check for duplicates.  This is an O(n^2) algorthm, but these
        // lists are usually quite short ...
        for (NamespaceBinding ns2 = bindings;  ; ns2 = ns2.getNext())
           {
             if (ns2 == ns)
               {
                 out.writeObject(prefix == null ? "" : prefix);
                 break;
               }
             if (ns2.getPrefix() == prefix)
               {
                 // Previously written.
                 break;
               }
           }
      }
  }

  public static void inScopePrefixes$X (Object node, CallContext ctx)
  {
    //if (node instanceof KElement)
      {
        KElement element = (KElement) node;
        Object type = element.sequence.getNextTypeObject(element.ipos);
        if (type instanceof XName)
          prefixesFromNodetype((XName) type, ctx.consumer);
        else
          ctx.consumer.writeObject("xml");
      }
  }

  public static void data$X (Object arg, CallContext ctx)
  {
    Consumer out = ctx.consumer; 
    if (arg instanceof Values)
      {
        Values vals = (Values) arg;
        int ipos = vals.startPos();
        while ((ipos = vals.nextPos(ipos)) != 0)
          out.writeObject(KNode.atomicValue(vals.getPosPrevious(ipos)));
      }
    else
      out.writeObject(KNode.atomicValue(arg));
  }

  /** Return the root node of the argument. */
  public static Object root (Object arg)
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("root", 1, arg, "node()?");
    KNode node = (KNode) arg;
    return Nodes.root((NodeTree) node.sequence, node.getPos());
  }

  public static String getLang (KNode node)
  {
    NodeTree seq = (NodeTree) node.sequence;
    int attr = seq.ancestorAttribute(node.ipos,
                                     gnu.xml.NamespaceBinding.XML_NAMESPACE,
                                     "lang");
    if (attr == 0)
      return null;
    else
      return KNode.getNodeValue(seq, attr);
  }

  public static boolean lang (Object testlang, Object node)
  {
    String teststr;
    if (testlang == null || testlang == Values.empty)
      teststr = "";
    else
      teststr = StringValue.stringValue(testlang);
    String lang = getLang((KNode) node);
    if (lang == null)
      return false;
    int langlen = lang.length();
    int testlen = teststr.length();
    if (langlen > testlen && lang.charAt(testlen) == '-')
      lang = lang.substring(0, testlen);
    return lang.equalsIgnoreCase(teststr);
  }

  public static Object documentUri (Object arg)
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("xs:document-uri", 1, arg, "node()?");
    KNode node = (KNode) arg;
    Object uri = ((NodeTree) node.sequence).documentUriOfPos(node.ipos);
    return uri == null ? Values.empty : uri;
  }

  public static Object nilled (Object arg)
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("nilled", 1, arg, "node()?");
    if (! (arg instanceof KElement))
      return Values.empty;
    return Boolean.FALSE;
  }

  public static Object baseUri (Object arg)
  /* #ifdef use:java.net.URI */
    throws java.net.URISyntaxException
  /* #end */
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("base-uri", 1, arg, "node()?");
    Object uri = ((KNode) arg).baseURI();
    return uri == null ? Values.empty : uri;
  }

  /** Extract canditate IDREFs from arg.
   * @return null (if no IDREFs); a String (if a single IDREF);
   *   or a Stack (if more than one IDREFs).
   */
  static Object getIDs (Object arg, Object collector)
  {
    if (arg instanceof KNode)
      arg = KNode.atomicValue(arg);
    if (arg instanceof Values)
      {
        Object[] ar = ((Values) arg).getValues();
        for (int i = ar.length; --i >= 0; )
          collector = getIDs(ar[i], collector);
      }
    else
      {
        String str = StringUtils.coerceToString(arg, "fn:id", 1, "");
        int len = str.length();
        int i = 0;
        while (i < len)
          {
            char ch = str.charAt(i++);
            if (Character.isWhitespace(ch))
              continue;
            int start = XName.isNameStart(ch) ? i - 1 : len;
            while (i < len)
              {
                ch = str.charAt(i);
                if (Character.isWhitespace(ch))
                  break;
                i++;
                if (start < len && ! XName.isNamePart(ch))
                  start = len;
              }
            if (start < len)
              {
                String ref = str.substring(start, i);
                if (collector == null)
                  collector = ref;
                else
                  {
                    Stack st;
                    if (collector instanceof Stack)
                      st = (Stack) collector;
                    else
                      {
                        st = new Stack();
                        st.push(collector);
                        collector = st;
                      }
                    st.push(ref);
                  }
              }
            i++;
          }
      }
    return collector;
  }

  public static void id$X (Object arg1, Object arg2, CallContext ctx)
  {
    KNode node = (KNode) arg2;
    NodeTree ntree = (NodeTree) node.sequence;
    KDocument root
      = (KDocument) Nodes.root(ntree, node.ipos);
    Consumer out = ctx.consumer;
    Object idrefs = getIDs(arg1, null);
    if (idrefs == null)
      return;
    ntree.makeIDtableIfNeeded();
    if (out instanceof PositionConsumer
        && (idrefs instanceof String || out instanceof SortedNodes))
      idScan(idrefs, ntree, (PositionConsumer) out);
    else if (idrefs instanceof String)
      {
        int pos = ntree.lookupID((String) idrefs);
        if (pos != -1)
          out.writeObject(KNode.make(ntree, pos));
      }
    else
      {
        SortedNodes nodes = new SortedNodes();
        idScan(idrefs, ntree, nodes);
        Values.writeValues(nodes, out);
      }
  }

  private static void idScan (Object ids, NodeTree seq, PositionConsumer out)
  {
    if (ids instanceof String)
      {
        int pos = seq.lookupID((String) ids);
        if (pos != -1)
          out.writePosition(seq, pos);
      }
    else if (ids instanceof Stack)
      {
        Stack st = (Stack) ids;
        int n = st.size();
        for (int i = 0;  i < n;  i++)
          idScan(st.elementAt(i), seq, out);
      }
  }

  public static Object idref (Object arg1, Object arg2)
  {
    KNode node = (KNode) arg2;
    KDocument root
      = (KDocument) Nodes.root((NodeTree) node.sequence, node.getPos());
    return Values.empty;
  }
}
