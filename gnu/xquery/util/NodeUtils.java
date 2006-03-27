// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.lists.Consumer;

public class NodeUtils
{
  public static Object localName (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
    Object name = NodeName.nodeName(node);
    if (name == null || name == Values.empty)
      return "";
    return QNameUtils.localNameFromQName(name);
  }

  public static Object namespaceURI (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
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
        out.writeObject(prefix == null ? "" : prefix);
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
}
