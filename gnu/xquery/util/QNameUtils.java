package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.XName;
import gnu.kawa.xml.*;

public class QNameUtils
{
  public static Object localNameFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    if (name instanceof XName)
      return ((XName) name).getLocalName();
    if (name instanceof Symbol)
      return ((Symbol) name).getName();
    throw WrongType.make(null, "local-name-from-QName", 1, name);
  }

  public static Object namespaceURIFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    if (name instanceof XName)
      return ((XName) name).getNamespaceURI();
    if (name instanceof Symbol)
      return ((Symbol) name).getNamespaceURI();
    throw WrongType.make(null, "namespace-uri", 1, name);
  }

  public static Object namespaceURIForPrefix (String prefix,
					      Object element)
  {
    KNode el = KNode.coerce(element);
    if (el == null)
      throw WrongType.make(null, "anmespace-uri-for-prefix", 2, element);
    String uri = el.lookupNamespaceURI(prefix);
    if (uri == null)
      return Values.empty;
    else
      return uri;
  }
}
