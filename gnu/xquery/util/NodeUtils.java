// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.*;

public class NodeUtils
{
  public static Object atomicValue (Object value)
  {
    if (value instanceof KNode)
      {
        KNode node = (KNode) value;
        return ((NodeTree) node.sequence).typedValue(node.ipos);
      }
    return value;
  }

  public static Object localName (Object node)
  {
    if (node == Values.empty || node == null)
      return node;
    Object name = NodeName.nodeName(node);
    return QNameUtils.localNameFromQName(name);
  }

  public static Object namespaceURI (Object node)
  {
    if (node == Values.empty || node == null)
      return node;
    Object name = NodeName.nodeName(node);
    return QNameUtils.namespaceURIFromQName(name);
  }
}
