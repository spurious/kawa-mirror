// Copyright (c) 1996-2000, 2002, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.io.*;

/** A Symbol is a name, usually in a specific Namespace.
 * A Symbol is stateless:  Comon Lisp-style "value", "function" and
 * "property list" bindings are not part of the Symbol itself, but
 * looked up in the current Environment.
 * A <code>Symbol</code> may be viewed as an <code>EnvironmentKey</code>
 * with a <code>null</code> property component.
 */

public class Symbol
  implements
  EnvironmentKey,
  /* #ifdef JAVA2 */
  Comparable,
  /* #endif */
  Externalizable
{
  static int counter;
  public int id = ++counter;

  protected String name;
  Namespace namespace;

  public final Symbol getKeySymbol () { return this; }
  public final Object getKeyProperty () { return null; }
  public boolean matches (EnvironmentKey key)
  {
    return key.getKeySymbol() == this && key.getKeyProperty() == null;
  }
  public boolean matches (Symbol symbol, Object property)
  {
    return symbol == this && property == null;
  }

  public final String getNamespaceURI()
  {
    Namespace ns = getNamespace();
    return ns == null ? null : ns.getName();
  }

  /** Synonym for getName - the "print name" of the symbol without Namespace.
   * Useful when thinking of a Symbol as an XML QName. */
  public final String getLocalName()
  {
    return name;
  }

  public final String getName()
  {
    return name;
  }

  /** Find or create a symbol in a specificed environment.
   * @param namespace can be an Namespace, or a namespace/environment name
   *   (resolved using Environment.getInstance), or null (in which case
   *   an uninterned symbol is created).
   * @param name The "local name" or "print name" of the desired symbol.
   */
  public static Symbol make (Object namespace, String name)
  {
    Namespace ns = namespace instanceof String
      ? Namespace.getInstance((String) namespace)
      : (Namespace) namespace;
    if (ns == null || name == null)
      return new Symbol(name, ns);
    return ns.getSymbol(name.intern());
  }

  public Symbol ()
  {
  }

  public Symbol (String name)
  {
    this.name = name;
  }

  public Symbol (String name, Namespace ns)
  {
    this.name = name;
    this.namespace = ns;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<symbol ");
    String name = getName();
    if (name != null)
      ps.print(name);
    ps.print ('>');
  }

  public int compareTo(Object o)
  {
    Symbol other = (Symbol) o;
    if (getNamespaceURI() != other.getNamespaceURI())
      throw new IllegalArgumentException("comparing Symbols in different namespaces");
    return getLocalName().compareTo(other.getLocalName());
  }

  /** Just tests for identity.
   * Otherwise hashTables that have Symbols as keys will break. */
  public boolean equals (Object o)
  {
    return this == o;
  }

  public int hashCode ()
  {
    return name == null ? 0 : name.hashCode();
  }


  public final Namespace getNamespace()
  {
    return namespace;
  }

  public final void setNamespace (Namespace ns)
  {
    namespace = ns;
  }

  /** Conventional value used as a property key for function bindings. */
  public static final Object FUNCTION = new Symbol("(function)", null);

  /** Conventional value used as a <code>Symbol</code> name to
   * access an <code>Object</code>'s property list.
   * A <dfn>property list</dfn> is a list with a even number of
   * <code>Pair</code>s, containing alternating keys and values.
   * They are used in Common Lisp and Emacs Lisp.
   * Kawa (following XEmacs) allows arbitrary objects to have property lists,
   * thus the PLIST as used as the name and the object as the property.
   * (In the future we'll do somethingg clever so that get(SYMBOL, KEY)
   * as the same as getf(get(PLIST, SYMBOL), KEY) - but much faster.)
   */
  public static final Symbol PLIST = new Symbol("(property-list)", null);

  public String toString()
  {
    // String h = "@"+Integer.toHexString(System.identityHashCode(this));
    String uri = getNamespaceURI();
    if (uri == null || uri.length() == 0)
      return getName();
    return '{'+getNamespaceURI()+"}"+getName();
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    Namespace ns = getNamespace();
    String name;
    if (ns != null && (name = ns.getName()) != null
	&& Namespace.getInstance(name) == ns)
      out.writeObject(name);
    else
      out.writeObject(ns);
    out.writeObject(getName());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    Object ns = in.readObject();
    namespace = (ns instanceof String ? Namespace.getInstance((String) ns)
		 : (Namespace) ns);
    name = (String) in.readObject();
  }

  public Object readResolve() throws ObjectStreamException
  {
    if (namespace == null)
      return this;
    return make(namespace, getName());
  }
}
