package gnu.kawa.xml;
import gnu.mapping.*;
import java.io.*;
import gnu.xml.NamespaceBinding;

public class XmlNamespace extends Namespace
  implements Externalizable
{
  public static final String XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";
  public static final XmlNamespace HTML = getInstance("", XHTML_NAMESPACE);
  public static final NamespaceBinding HTML_BINDINGS =
    new NamespaceBinding(null, XHTML_NAMESPACE,
                         NamespaceBinding.predefinedXML);

  public static XmlNamespace getInstance (String prefix, String uri)
  {
    String xname = prefix + " [xml] -> "+ uri;
    synchronized (nsTable)
      {
	Object old = nsTable.get(xname);
	if (old instanceof XmlNamespace)
	  return (XmlNamespace) old;
	XmlNamespace ns = new XmlNamespace();
        ns.setName(uri.intern());
        ns.prefix = prefix.intern();
	nsTable.put(xname, ns);
	return ns;
      }
  }

  /** Emitted by compiler to handle literals. */
  public static XmlNamespace make (String prefix, String name)
  {
    return getInstance(prefix, name);
  }

  public Object get (String name)
  {
    ElementType type = ElementType.make(getSymbol(name));
    if (this == XmlNamespace.HTML)
      type.setNamespaceNodes(XmlNamespace.HTML_BINDINGS);
    return type;
  }

  public boolean isConstant (String key)
  {
    return true;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
    out.writeObject(prefix);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setName((String) in.readObject());
    prefix = (String) in.readObject();
  }

  public Object readResolve() throws ObjectStreamException
  {
    String xname = prefix + " -> "+ getName();
    Namespace ns = (Namespace) nsTable.get(xname);
    if (ns instanceof XmlNamespace)
      return ns;
    nsTable.put(xname, this);
    return this;
  }
}
