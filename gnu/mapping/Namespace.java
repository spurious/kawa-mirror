// Copyright (c) 1996-2000, 2001, 2002, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.util.Hashtable;
import java.io.*;

/** A mapping from strings ("print names") to <code>Symbol</code>s.
 * Namespaces are normally named and can be accessed from a global table.
 * They correspond to Common Lisp "packages" (which are implemented
 * using <code>gnu.kawa.lispexpr.LispPackage</code>,
 * which extends <code>Namespace</code>).
 * A <code>Namespace</code> is a "weak" mapping in the sense that a
 * <code>Symbol</code> can be garbage collected even though it is
 * referenced from a <code>Namespace</code>.
 * @author	Per Bothner
 */

public class Namespace
  implements Externalizable, HasNamedParts
{
  /** Map namepsace names (and nick-names) to Namespaces. */
  static final Hashtable nsTable = new Hashtable(50);

  /** The Namespace with the empty name. */
  public static final Namespace EmptyNamespace = getInstance("");

  String name;

  /** Get the name of this Namespace. */
  public final String getName () { return name; }
  /** Set the name of this Namespace. */
  public final void setName (String name) { this.name = name; }

  public Namespace ()
  {
    this(64);
  }

  public Namespace (int capacity)
  {
    log2Size = 4;
    while (capacity > (1 << log2Size))
      log2Size++;
    capacity = 1 << log2Size;
    table = new SymbolRef[capacity];
    mask = capacity - 1;
  }

  /** A kludge used for serializing SName. */
  protected Namespace (String name, int dummy)
  {
    this.name = name;
  }

  public static Namespace getDefault ()
  {
    return EmptyNamespace;
  }

  public static Symbol getDefaultSymbol (String name)
  {
    return EmptyNamespace.getSymbol(name);
  }

  public static Namespace make(String name) // Needed for Literals.
  {
    return getInstance(name);
  }

  public static Namespace getInstance(String name)
  {
    if (name == null)
      name = "";
    synchronized (nsTable)
      {
	Namespace ns = (Namespace) nsTable.get(name);
	if (ns != null)
	  return ns;
	ns = new Namespace ();
	ns.setName(name);
	nsTable.put(name, ns);
	return ns;
      }
  }

  public Object get (String name)
  {
    return Environment.getCurrent().get(getSymbol(getName()));
  }

  /** Get a Symbol matching the given name.
   * Creates a new Symbol if one is not found.
   * Equivalent to Common Lisp's "intern" function.
   */
  public Symbol getSymbol (String key)
  {
    return lookup(key, key.hashCode(), true);
  }

  /** Get a Symbol matching the given name.
   * Returns null if one is not found.
   */
  public Symbol lookup(String key)
  {
    return lookup(key, key.hashCode(), false);
  }

  protected final Symbol lookupInternal(String key, int hash)
  {
    int index = hash & mask;
    SymbolRef prev = null;
    for (SymbolRef ref = table[index];  ref != null;  )
      {
	SymbolRef next = ref.next;
	Symbol sym = ref.getSymbol();
	if (sym == null)
	  {
	    // Weakly referenced object has been collected.
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.next = next;
	    num_bindings--;
	  }
	else
	  {
	    if (sym.name.equals(key))
	      return sym;
	    prev = ref;
	  }
	ref = next;
      }
    return null;
  }

  public Symbol add(Symbol sym, int hash)
  {
    int index = hash & mask;
    SymbolRef ref = new SymbolRef(sym, this);
    sym.namespace = this;
    ref.next = table[index];
    table[index] = ref;
    num_bindings++;
    if (num_bindings >= table.length)
      rehash();
    return sym;
  }

  public Symbol lookup(String key, int hash, boolean create)
  {
    synchronized (this)
      {
	Symbol sym = lookupInternal(key, hash);
	if (sym != null)
	  return sym;

	/*
	// Do we need to synchronize on nsTable as well?  FIXME
	for (NamespaceUse used = imported;  used != null;
	     used = used.nextImported)
	  {
	    el = lookup(key, hash, false);
	    if (el != null)
	      return el;
	  }
	*/
	if (create)
	  return add(new Symbol(key), hash);
	else
	  return null;
      }
  }

  public boolean remove (Symbol symbol)
  {
    synchronized (this)
      {
	String name = symbol.name;
	int hash = name.hashCode();
	int index = hash & mask;
	SymbolRef prev = null;
	SymbolRef ref = table[index];
	while (ref != null)
	  {
	    SymbolRef next = ref.next;
	    Symbol refsym = ref.getSymbol();
	    if (refsym == null || refsym == symbol)
	      {
		if (prev == null)
		  table[index] = next;
		else
		  prev.next = next;
		num_bindings--;
		if (refsym != null)
		  return true;
	      }
	    else
	      prev = ref;
	    ref = next;
	  }
	return false;
      }
  }

  protected SymbolRef[] table;
  int log2Size;
  private int mask;
  int num_bindings;

  protected void rehash ()
  {
    int oldCapacity = table.length;
    int newCapacity = 2 * oldCapacity;
    int newMask = newCapacity - 1;
    int countInserted = 0;
    SymbolRef[] oldTable = table;
    SymbolRef[] newTable = new SymbolRef[newCapacity];

    for (int i = oldCapacity;  --i >= 0;)
      {
	for (SymbolRef ref = oldTable[i];  ref != null;  )
	  {
	    SymbolRef next = ref.next;
	    Symbol sym = ref.getSymbol();
	    if (sym != null)
	      {
		String key = sym.getName();
		int hash = key.hashCode();
		int index = hash & newMask;
		countInserted++;
		ref.next = newTable[index];
		newTable[index] = ref;
	      }
	    ref = next;
	  }
      }

    table = newTable;
    log2Size++;
    mask = newMask;
    num_bindings = countInserted;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setName((String) in.readObject());
  }

  public Object readResolve() throws ObjectStreamException
  {
    String name = getName();
    if (name != null)
      {
	Namespace ns = (Namespace) nsTable.get(name);
	if (ns != null)
	  return ns;
	nsTable.put(name, this);
      }
    return this;
   
  }

  public String toString()
  { return "#,(namespace \""+name+"\")"; }
}

/** A week reference to a <code>Symbol</code>.
 * This is to allow <code>Symbol</code>s to be garbage collected,
 * even though they're referenced from a <code>Namespace</code>. */

class SymbolRef
  /* #ifdef JAVA2 */
  extends java.lang.ref.WeakReference
  /* #endif */
{
  SymbolRef next;

  /* #ifndef JAVA2 */
  // Symbol sym;
  /* #endif */

  /*
  String getName ()
  {
    Symbol sym = getSymbol();
    return sym == null ? null : sym.getName();
  }
  */

  SymbolRef (Symbol sym, Namespace ns)
  {
    /* #ifdef JAVA2 */
    super(sym);
    /* #endif */
    /* #ifndef JAVA2 */
    // this.sym = sym;
    /* #endif */
  }

  Symbol getSymbol()
  {
    /* #ifdef JAVA2 */
    return (Symbol) get();
    /* #endif */
    /* #ifndef JAVA2 */
    // return sym;
    /* #endif */
  }

  public String toString()
  {
    return "SymbolRef["+getSymbol()+"]";
  }
}
