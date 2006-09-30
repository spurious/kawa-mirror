package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.*;

public class QNameUtils
{
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
    if (constructorNamespaces == null)
      uri = null;
    else
      uri = constructorNamespaces.resolve(prefix);
    if (uri == null)
      uri = prologNamespaces.resolve(prefix);
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
    // FIXME should check the syntax is valid for an NCName.
    return true;
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
      return ((Symbol) name).getPrefix();
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
