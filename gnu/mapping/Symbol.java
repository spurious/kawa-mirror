// Copyright (c) 1996-2000, 2002, 2004, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.io.*;

// Enable JAXP-QName if Symbol should extend javax.xml.namespace.QName.
// This is not the default, even when JAXP-1.3 is defined, because it makes
// symbols bigger and some operations (such as equals and handling of
// uninterned symbols) slower, without much benefit.
// This option needs some work.
/* #ifdef JAXP-QName */
// import javax.xml.namespace.QName;
/* #endif */

/** A Symbol is a name, usually in a specific Namespace.
 * A Symbol is stateless:  Comon Lisp-style "value", "function" and
 * "property list" bindings are not part of the Symbol itself, but
 * looked up in the current Environment.
 * A <code>Symbol</code> may be viewed as an <code>EnvironmentKey</code>
 * with a <code>null</code> property component.
 */

public class Symbol
  /* #ifdef JAXP-QName */
  // extends QName
  /* #endif */
  implements
  EnvironmentKey,
  /* #ifdef JAVA2 */
  Comparable,
  /* #endif */
  Externalizable
{
  /* #ifndef JAXP-QName */
  protected String name;
  /* #endif */
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

  /* #ifndef JAXP-QName */
  public final String getNamespaceURI()
  {
    Namespace ns = getNamespace();
    return ns == null ? null : ns.getName();
  }

  public final String getLocalPart()
  {
    return name;
  }

  public final String getPrefix ()
  {
    Namespace ns = namespace;
    return ns == null ? "" : ns.prefix;
  }
  /* #endif */

  public final boolean hasEmptyNamespace ()
  {
    Namespace ns = getNamespace();
    String nsname;
    return (ns == null
	    || (nsname = ns.getName()) == null || nsname.length() == 0);
  }

  /** Synonym for getName - the "print name" of the symbol without Namespace.
   * Useful when thinking of a Symbol as an XML QName. */
  public final String getLocalName()
  {
    /* #ifdef JAXP-QName */
    // return getLocalPart();
    /* #else */
    return name;
    /* #endif */
  }

  public final String getName()
  {
    /* #ifdef JAXP-QName */
    // return getLocalPart();
    /* #else */
    return name;
    /* #endif */
  }

  public static Symbol make (String uri, String name, String prefix)
  {
    return Namespace.make(uri, prefix).getSymbol(name.intern());
  }

  /** Find or create a symbol in a specificed environment.
   * @param namespace can be an Namespace, or a namespace/environment name
   *   (resolved using Environment.getInstance), or null (in which case
   *   an uninterned symbol is created).
   * @param name The "local name" or "print name" of the desired symbol.
   * @param prefix namespace prefix, or null
   */
  public static Symbol make (Object namespace, String name)
  {
    Namespace ns = namespace instanceof String
      ? Namespace.getInstance((String) namespace)
      : (Namespace) namespace;
    if (ns == null || name == null)
      return makeUninterned(name);
    return ns.getSymbol(name.intern());
  }

  public Symbol ()
  {
    /* #ifdef JAXP-QName */
    // super("");
    /* #endif */
  }

  public static Symbol makeUninterned (String name)
  {
    /* #ifdef JAXP-QName */
    // Namespace ns = Namespace.getInstance("kawa.gensym");
    // String sname = name;
    // int i = 0;
    // for (;;)
    //   {
    //     int hash = sname.hashCode();
    //     synchronized (ns)
    //       {
    //         Symbol sym = ns.lookup(sname, hash, false);
    //         if (sym == null)
    //           return ns.add(new Symbol(ns, sname.intern()), hash);
    //       }
    //     sname = name + '.' + ++i;
    //   }
    /* #else */
    return new Symbol(null, name);
    /* #endif */
  }

  /** Create new Symbol in a given namespace.
   * Does not enter the result in the namespace's symbol table.
   * @param name an interned String
   */
  public Symbol (Namespace ns, String name)
  {
    /* #ifdef JAXP-QName */
    // super(ns == null ? "" : ns.getName(), name, ns == null ? "" : ns.prefix);
    /* #else */
    this.name = name;
    /* #endif */
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

  public static boolean equals (Symbol sym1, Symbol sym2)
  {
    if (sym1 == sym2)
      return true;
    /* #ifdef JAXP-QName */
    // if (sym1.getLocalPart() == sym2.getLocalPart())
    /* #else */
    if (sym1.name == sym2.name)
    /* #endif */
      {
        Namespace namespace1 = sym1.namespace;
        Namespace namespace2 = sym2.namespace;
        return (namespace1 != null && namespace2 != null
                && (namespace1 == namespace2
                    || namespace1.name == namespace2.name));
      }
    return false;
  }

  /* #ifndef JAXP-QName */
  /** Just tests for identity.
   * Otherwise hashTables that have Symbols as keys will break. */
  public final boolean equals (Object o)
  {
    return o instanceof Symbol && equals(this, (Symbol) o);
  }

  public int hashCode ()
  {
    return name == null ? 0 : name.hashCode();
  }
  /* #endif */

  public final Namespace getNamespace()
  {
    return namespace;
  }

  public final void setNamespace (Namespace ns)
  {
    namespace = ns;
  }

  /** Conventional value used as a property key for function bindings. */
  public static final Symbol FUNCTION = makeUninterned("(function)");

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
  public static final Symbol PLIST = makeUninterned("(property-list)");

  public String toString()
  {
    // String h = "@"+Integer.toHexString(System.identityHashCode(this));
    String uri = getNamespaceURI();
    if (uri == null || uri.length() == 0)
      return getName();
    StringBuffer sbuf = new StringBuffer();
    String prefix = getPrefix();
    if (prefix == null || prefix.length() == 0)
      {
        sbuf.append('{');
        sbuf.append(getNamespaceURI());
        sbuf.append('}');
      }
    else
      {
        sbuf.append(prefix);
        sbuf.append(':');
      }
    sbuf.append(getName());
    return sbuf.toString();
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    Namespace ns = getNamespace();
    out.writeObject(ns);
    out.writeObject(getName());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    /* #ifdef JAXP-QName */
    // throw new Error("Symbol.readExternal not implemented"); // FIXME!
    /* #else */
    namespace = (Namespace) in.readObject();
    name = (String) in.readObject();
    /* #endif */
  }

  public Object readResolve() throws ObjectStreamException
  {
    if (namespace == null)
      return this;
    return make(namespace, getName());
  }
}
