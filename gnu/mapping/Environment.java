// Copyright (c) 1996-2000, 2001, 2002, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.util.Hashtable;
import java.io.*;

/**
 * An environment contains (name->value) bindings.
 * Names are Strings that are compared by ==, not equal.
 * @author	Per Bothner
 */

public class Environment extends NameMap implements Externalizable
{
  Symbol[] table;
  int log2Size;
  private int mask;
  int num_bindings;

  static Environment global;

  Environment previous;

  boolean locked;

  protected TrivialConstraint trivialConstraint = new TrivialConstraint(this);
  protected UnboundConstraint unboundConstraint = new UnboundConstraint(this);
  protected ConstantConstraint constantConstraint;

  /** True if this environment is locked - bindings cannot be added or removed. */
  public final boolean isLocked()
  {
    return locked;
  }

  public final void setLocked(boolean locked)
  {
    this.locked = locked;
  }

  public Environment getPrevious ()
  {
    return previous;
  }

  public void setPrevious (Environment previous)
  {
    this.previous = previous;
  }

  static final Hashtable envTable = new Hashtable(50);
  static final Environment EmptyNamespace = getInstance("");

  public static Environment getInstance(String name)
  {
    if (name == null)
      name = "";
    synchronized (envTable)
      {
	Environment env = (Environment) envTable.get(name);
	if (env != null)
	  return env;
	env = new Environment ();
	env.setName(name);
	envTable.put(name, env);
	return env;
      }
  }

  public static Environment user () { return getCurrent(); }

  public static Object lookup_global (String name)
       throws UnboundSymbol
  {
    Symbol binding = getCurrent().lookup(name);
    if (binding == null)
      throw new UnboundSymbol(name);
    return binding.get ();
  }

  /** Define name (interned) to have a given value. */
  public static void define_global (String name, Object new_value)
  {
    getCurrent().defineValue(name, new_value);
  }

  public static void defineFunction (String name, Object new_value)
  {
    defineFunction(user(), name, new_value);
  }

  public static void defineFunction (Environment env,
				     String name, Object new_value)
  {
    Symbol binding = env.getSymbol(name);
    binding.constraint.setFunctionValue(binding, new_value);
  }

  /** Define name (interned) to have a given value. */
  public static void put_global (String name, Object new_value)
  {
    getCurrent().put (name, new_value);
  }

  /**
    * @deprecated
    */
  public static Environment current () { return getCurrent(); }
  public static Environment getCurrent ()
  {
    return CallContext.getInstance().getEnvironment();
  }

  public static void setCurrent (Environment env)
  {
    CallContext ctx = CallContext.getInstance();
    ctx.curEnvironment = env;
  }

  public static void setGlobal (Environment env)
  {
    global = env;
  }

  public Environment ()
  {
    this(64);
  }

  public Environment (String name)
  {
    this();
    setName(name);
  }

  public Environment (int capacity)
  {
    log2Size = 4;
    while (capacity > (1 << log2Size))
      log2Size++;
    capacity = 1 << log2Size;
    table = new Symbol[capacity];
    mask = capacity - 1;
  }

  public Environment (Environment previous)
  {
    this ();
    this.previous = previous;
  }

  public synchronized Symbol getSymbol (String name)
  {
    Symbol binding = lookup(name);
    if (binding != null)
      return binding;
    /* FIXME
    int hash = System.identityHashCode(name);
    int index = Symbol.hashSearch(table, log2Size,mask, name, hash);
    Symbol binding = table[index];
    if (binding != null && binding != Symbol.hashDELETED)
      return binding;
    if (locked)
      {
	if (previous == null)
	  throw new UnboundSymbol(name);
	return previous.getSymbol(name);
      }
    */
    binding = addSymbol(name, null);
    binding.constraint = unboundConstraint;
    return binding;
  }

  public static Symbol getCurrentSymbol (String name)
  {
    return getCurrent().getSymbol(name);
  }

  /**
   * Search for a variable binding by name.
   * @param sym the (interned) name of the binding to search for
   * @return the value of the binding, or null if not found
   */

  public Symbol lookup (String name)
  {
    return lookup(name, System.identityHashCode(name));
  }

  private Symbol lookup (String name, int hash)
  {
    for (Environment env = this;  env != null;  )
      {
	synchronized (env)
	  {
	    int index = Symbol.hashSearch(env.table, env.log2Size, env.mask,
					  name, hash);
	    Symbol element = env.table[index];
	    if (element != null && element != Symbol.hashDELETED)
		return element;
	    env = env.previous;
	  }
      }
    return null;
  }

  /**
   * Define the value binding for a symbol.
   */
  public Symbol defineValue (String name, Object value)
  {
    Symbol binding = getSymbol(name);
    binding.constraint = trivialConstraint;
    binding.value = value;
    return binding;
  }

  /**
   * Define the value or function binding for a symbol, as appropriate
   */
  public Symbol define (String name, Object value)
  {
    return defineValue(name, value);
  }

  public synchronized void addSymbol(Symbol binding)
  {
    // Rehash if over 2/3 full.
    if (3 * num_bindings >= 2 * table.length)
      rehash();
    if (Symbol.hashSet(table, log2Size, binding) == null)
      num_bindings++;
  }

  public Symbol addSymbol (String name, Object value)
  {
    Symbol binding = new Symbol(name);
    binding.constraint = trivialConstraint;
    binding.value = value;
    addSymbol(binding);
    return binding;
  }

  void rehash ()
  {
    int new_capacity = 2 * table.length;
    Symbol[] new_table = new Symbol[new_capacity];

    Symbol.hashInsertAll(new_table, log2Size + 1,
			  table, log2Size);
    table = new_table;
    log2Size++;
    mask = (mask << 1) | 1;
  }

  public Object remove (String name)
  {
    Environment env = this;
    for (;;)
      {
	if (env == null)
	  return null;
	if (locked)
	  throw new IllegalStateException("attempt to remove variable: "
					  + name + " locked environment");
	Environment previous;
	synchronized (env)
	  {
	    Symbol[] env_tab = env.table;
	    Named old = Symbol.hashDelete(env.table, env.log2Size, name);
	    if (old != null)
		return old;
	    previous = env.previous;
	  }
	env = previous;
      }
  }

  public Object remove (Object name)
  {
    return remove((String) name);
  }

  public synchronized void remove (Symbol binding)
  {
    String name = binding.getName();
    if (locked)
      throw new IllegalStateException("attempt to remove variable: "
				      + name + " locked environment");
    Symbol.hashDelete(table, log2Size, name);
  }

  public final boolean isBound(String name)
  {
    return get(name, Symbol.UNBOUND) != Symbol.UNBOUND;
  }

  public Object get(String name, Object defaultValue)
  {
    Symbol binding = lookup(name);
    if (binding == null)
      return defaultValue;
    return binding.get(defaultValue);
  }

  /** Get the function binding for a symbol.
   * If this Environment is a single-namespace language (such as Scheme).
   * this is equivalent to getChecked.
   * @exception gnu.mapping.UnboundSymbol the name has no function binding
   */
  public Object getFunction(String name)
  {
    return getChecked(name);
  }

  public Object put (/* interned */ String name, Object value)
  {
    Symbol binding = lookup (name);
    if (binding == null)
      {
	define (name, value);
	return null;
      }
    else if (! binding.isBound())
      {
	binding.set (value);
        return null;
      }
    else
      {
	Object old_value = binding.get ();
	binding.set (value);
	return old_value;
      }
  }

  public Object put (Object name, Object value)
  {
    return put ((String) name, value);
  }

  /** Set the function binding for a symbol.
   * If this Environment is a single-namespace language (such as Scheme).
   * this is equivalent to put.
   */
  public void putFunction(String name, Object value)
  {
    put(name, value);
  }

  /** Does not enumerate inherited Symbols. */
  public SymbolEnumeration enumerateSymbols()
  {
    return new SymbolEnumeration(table, 1 << log2Size);
  }

  /** Does enumerate inherited Symbols. */
  public SymbolEnumeration enumerateAllSymbols()
  {
    return new SymbolEnumeration(this);
  }

  public String toString ()
  {
    String name = getName();
    if (name == null)
      name = super.toString ();
    return "#<environment " + name + '>';
  }

  /**
   * Evaluate an expression in this Environment.
   */
  /*
  final public Object
  eval (Expression expr)
  {
    return expr.eval (this);
  }
  */

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
    Environment env = (Environment) envTable.get(name);
    if (env != null)
      return env;
    envTable.put(name, this);
    return this;
   
  }
}
