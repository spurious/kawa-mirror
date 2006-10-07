// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.lists.Consumer;

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
}
