package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.*;

public class QNameUtils
{
  public static Object resolveQNameUsingElement (Object qname, KElement node)
  {
    qname = KNode.atomicValue(qname);
    if (qname == Values.empty || qname == null)
      return qname;
    if (qname instanceof Values
        || ! (qname instanceof String || qname instanceof UntypedAtomic))
      throw new RuntimeException("bad argument to QName");
    String name = qname.toString();
    int colon = name.indexOf(':');
    String prefix, localPart, uri;
    if (colon < 0)
      {
	localPart = name;
	prefix = null;
        uri = "";
      }
    else
      {
	prefix = name.substring(0, colon).intern();
	localPart = name.substring(colon+1);
        uri =  node.lookupNamespaceURI(prefix);
      }
    if (! validNCName(localPart)
	|| (prefix != null && ! validNCName(prefix)))
      {
	throw new RuntimeException("invalid QName syntax '"+name+"'");
      }
    return Symbol.make(uri, localPart, prefix == null ? "" : prefix);
  }

  /** Method called from compiled code to "cast" to a QName.
   * @param qname The value to cast to QName.
   * @param constructorNamespaces Namespace bindings from namespace
   *   attributes in direct element constructors.
   * @param prologNamespaces Namespac bindings from query prolog,
   *   as well as builtin namespace prefixes.
   */
  public static Object resolveQName (Object qname,
				     NamespaceBinding constructorNamespaces,
				     NamespaceBinding prologNamespaces)
  {
    qname = KNode.atomicValue(qname);
    if (qname instanceof Symbol)
      return qname;
    if (qname instanceof Values
        || ! (qname instanceof String || qname instanceof UntypedAtomic))
      throw new RuntimeException("bad argument to QName");
    String name = qname.toString();
    int colon = name.indexOf(':');
    String prefix, localPart;
    if (colon < 0)
      {
	localPart = name;
	prefix = null;
      }
    else
      {
	prefix = name.substring(0, colon).intern();
	localPart = name.substring(colon+1);
      }
    if (! validNCName(localPart)
	|| (prefix != null && ! validNCName(prefix)))
      {
	throw new RuntimeException("invalid QName syntax '"+name+"'");
      }
    String uri = resolvePrefix(prefix, constructorNamespaces, prologNamespaces);
    return Symbol.make(uri, localPart, prefix == null ? "" : prefix);
  }

  public static String resolvePrefix (String prefix,
                                      NamespaceBinding constructorNamespaces,
                                      NamespaceBinding prologNamespaces)
  {
    String uri;

    for (NamespaceBinding ns = constructorNamespaces; ; ns = ns.getNext())
      {
        if (ns == null)
          {
            uri = prologNamespaces.resolve(prefix);
            break;
          }
	if (ns.getPrefix() == prefix || ns.getUri() == null)
          {
            uri = ns.getUri();
            break;
          }
      }
    if (uri == null)
      {
	if (prefix == null)
	  uri = "";
	else
	  throw new RuntimeException("unknown namespace prefix '"+prefix+"'");
      }
    return uri;
  }

  public static boolean validNCName (String name)
  {
    return XName.isName(name);
  }

  /** This implements the <code>fn:QName</code> standard function. */

  public static Symbol makeQName (Object paramURI, String paramQName)
  {
    if (paramURI == null || paramURI == Values.empty)
      paramURI = "";
    int colon = paramQName.indexOf(':');
    String namespaceURI = (String) paramURI, localPart, prefix;
    if (colon < 0)
      {
	localPart = paramQName;
	prefix = "";
      }
    else
      {
	localPart = paramQName.substring(colon+1);
	prefix = paramQName.substring(0, colon).intern();
      }
    if (! validNCName(localPart)
	|| (colon >= 0 && ! validNCName(prefix)))
      throw new IllegalArgumentException("invalid QName syntax '"+paramQName+"'");
    if (colon >= 0 && namespaceURI.length() == 0)
      throw new IllegalArgumentException("empty uri for '"+paramQName+"'");
    return Symbol.make(namespaceURI, localPart, prefix);
  }

  public static Object localNameFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    if (name instanceof Symbol)
      return ((Symbol) name).getName();
    throw WrongType.make(null, "local-name-from-QName", 1, name);
  }

  public static Object prefixFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    if (name instanceof Symbol)
      {
        String prefix = ((Symbol) name).getPrefix();
        if (prefix == null || prefix.length() == 0)
          return Values.empty;
        return prefix;
      }
    throw WrongType.make(null, "prefix-from-QName", 1, name);
  }

  public static Object namespaceURIFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    if (name instanceof Symbol)
      return ((Symbol) name).getNamespaceURI();
    throw WrongType.make(null, "namespace-uri", 1, name);
  }

  public static Object namespaceURIForPrefix (Object prefix,
					      Object element)
  {
    KNode el = KNode.coerce(element);
    if (el == null)
      throw WrongType.make(null, "namespace-uri-for-prefix", 2, element);
    if (prefix == null || prefix == Values.empty)
      prefix = "";
    else if (! (prefix instanceof String || prefix instanceof UntypedAtomic))
      throw WrongType.make(null, "namespace-uri-for-prefix", 1, element);
    String uri = el.lookupNamespaceURI(prefix.toString());
    if (uri == null)
      return Values.empty;
    else
      return uri;
  }
}
