package gnu.mapping;

/**
 * An environment contains (name->value) bindings.
 * Names are Strings that are compared by ==, not equal.
 * @author	Per Bothner
 */

public class Environment extends NameMap
{
  private Binding[] table;

  private static Environment global;

  Environment previous;

  protected TrivialConstraint trivialConstraint = new TrivialConstraint(this);
  protected UnboundConstraint unboundConstraint = new UnboundConstraint(this);

  static final float threshold = (float) 0.7;
  static int num_bindings;

  public static Environment user () { return current(); }

  public static Object lookup_global (String name)
       throws UnboundSymbol
  {
    Binding binding = user().lookup(name);
    if (binding == null)
      throw new UnboundSymbol(name);
    return binding.get ();
  }

  /** Define name (interned) to have a given value. */
  public static void define_global (String name, Object new_value)
  {
    user().define (name, new_value);
  }

  /** Define name (interned) to have a given value. */
  public static void put_global (String name, Object new_value)
  {
    user().put (name, new_value);
  }

  /**
    * @deprecated
    */
  public static Environment current () { return getCurrent(); }
  public static Environment getCurrent ()
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      return ((Future)thread).environment;
    return global;
  }

  public static void setCurrent (Environment env)
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      ((Future) thread).environment = env;
    else
      global = env;
  }

  public Environment ()
  {
    this (60);
  }

  public Environment (int initialCapacity)
  {
    table = new Binding[initialCapacity];
  }

  public Environment (Environment previous)
  {
    this ();
    this.previous = previous;
  }

  public Binding getBinding (String name)
  {
    Binding binding = lookup(name);
    if (binding != null)
      return binding;
    binding = addBinding(name, null);
    binding.constraint = unboundConstraint;
    return binding;
  }

  public static Binding getCurrentBinding (String name)
  {
    return getCurrent().getBinding(name);
  }

  /**
   * Search for a variable binding by name.
   * @param sym the (interned) name of the binding to search for
   * @return the value of the binding, or null if not found
   */

  public Binding lookup (String name)
  {
    return lookup(name, System.identityHashCode(name));
  }

  private Binding lookup (String name, int hash)
  {
    for (Environment env = this;  env != null;  env = env.previous)
      {
	Binding[] env_tab = env.table;
	int index = (hash & 0x7FFFFFFF) % env_tab.length;
	for (Binding binding = env_tab[index];
	     binding != null;  binding = binding.chain)
	  {
	    if (binding.sym_name == name)
	      return binding;
	  }
      }
    return null;
  }

  public Binding define (String name, Object value)
  {
    Binding binding = getBinding(name);
    binding.set(value);
    return binding;
  }

  public Binding addBinding (String name, Object value)
  {
    if (num_bindings >= table.length * threshold)
      rehash (2 * table.length);

    num_bindings++;
    int hash = System.identityHashCode(name);
    int index = (hash & 0x7FFFFFFF) % table.length;

    Binding binding = new Binding(name);
    binding.constraint = trivialConstraint;
    binding.value = value;
    binding.chain = table[index];
    table[index] = binding;
    return binding;
  }

  void rehash (int new_capacity)
  {
    Binding[] new_table = new Binding[new_capacity];
    for (int i = table.length;  --i >= 0;)
      {
	// First reverse the chain of bindings, do we can handle oldest first
	Binding prev = null;
	for (Binding cur = table[i];  cur != null; )
	  {
	    Binding next = cur.chain;
	    cur.chain = prev;
	    prev = cur;
	    cur = next;
	  }
	table[i] = prev;

	for (Binding cur = table[i];  cur != null; )
	  {
	    int hash = System.identityHashCode(cur.sym_name);
	    int new_index = (hash & 0x7FFFFFFF) % new_capacity;
	    Binding next = cur.chain;
	    cur.chain = new_table[new_index];
	    new_table[new_index] = cur;
	    cur = next;
	  }
      }
    table = new_table;
  }

  public Object remove (String name)
  {
    int hash = System.identityHashCode(name);
    Environment env = this;
    for ( ; ;  env = env.previous)
      {
	if (env == null)
	  return null;
	Binding[] env_tab = env.table;
	int index = (hash & 0x7FFFFFFF) % env_tab.length;
	for (Binding binding = env_tab[index];
	     binding != null;  binding = binding.chain)
	  {
	    if (binding.sym_name == name)
	      {
		Object old = binding.get(); 
		env.remove(binding);
		return old;
	      }
	  }
      }
  }

  public Object remove (Object name)
  {
    return remove((String) name);
  }

  public void remove (Binding binding)
  {
    int hash = System.identityHashCode(binding.sym_name);
    int index = (hash & 0x7FFFFFFF) % table.length;
    Binding prev = null;
    for (Binding b = table[index];  b != null ; )
      {
	Binding next = b.chain;
	if (b == binding)
	  {
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.chain = next;
	    num_bindings--;
	    return;
	  }
	prev = b;
	b = next;
      }
  }

  /** Get the value bound to the given name.
   * @exception gnu.mapping.UnboundSymbol the name has no binding
   * @see Environment#get(Object)
   */
  public Object getChecked(String name)
  {
    Binding binding = lookup (name);
    if (binding == null)
      throw new UnboundSymbol(name);
    return binding.get ();
  }

  public Object put (/* interned */ String name, Object value)
  {
    Binding binding = lookup (name);
    if (binding == null)
      {
	define (name, value);
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
}
