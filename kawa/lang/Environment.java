package kawa.lang;

/**
 * An environment contains (name->value) bindings.
 * Names are Strings that are compared by ==, not equal.
 * @author	Per Bothner
 */

public class Environment // extends [almost] java.util.Dictionary
{
  private Binding[] table;

  Environment previous;

  int time_stamp;

  String name;

  /** The value of previous.time_stamp when this was created.
   * Newer Bindings in previous are ignored. */
  int previous_time_stamp;

  static final float threshold = (float) 0.7;
  static int num_bindings;

  public final String getName ()
  {
    return name;
  }

  public final void setName (String name)
  {
    this.name = name;
  }

  public static Environment user () { return current(); }

  public static Object lookup_global (String name)
       throws UnboundSymbol
  {
    Object result = user().get (name);
    if (result == null)
      throw new UnboundSymbol(name);
    return result;
  }

  /** Define name (interned) to have a given value. */
  public static void define_global (String name, Object new_value)
  {
    user().define (name, new_value);
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
    return kawa.standard.Scheme.curEnvironment ();
  }

  public static void setCurrent (Environment env)
  {
    Thread thread = Thread.currentThread ();
    if (thread instanceof Future)
      {
	((Future)thread).environment = env;
      }
    else
      kawa.standard.Scheme.setEnvironment (env);
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
    this.previous_time_stamp = this.time_stamp = previous.time_stamp;
  }

  /**
   * Search for a variable binding by name.
   * @param sym the (interned) name of the binding to search for
   * @return the value of the binding, or null if not found
   */

  public Binding lookup (String name)
  {
    int time_stamp = this.time_stamp;
    int hash = name.hashCode ();
    for (Environment env = this;  env != null;  env = env.previous)
      {
	Binding[] env_tab = env.table;
	int index = (hash & 0x7FFFFFFF) % env_tab.length;
	for (Binding binding = env_tab[index];
	     binding != null;  binding = binding.chain)
	  {
	    if (binding.name == name && binding.time_stamp < time_stamp)
	      return binding;
	  }
	time_stamp = env.previous_time_stamp;
      }
    return null;
  }

  public Binding define (String name, Object value)
  {
    if (num_bindings >= table.length * threshold)
      rehash (2 * table.length);

    num_bindings++;
    int hash = name.hashCode ();
    int index = (hash & 0x7FFFFFFF) % table.length;

    Binding binding = new Binding ();
    binding.name = name;
    binding.value = value;
    binding.time_stamp = time_stamp++;
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
	    int hash = cur.name.hashCode ();
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
    int time_stamp = this.time_stamp;
    int hash = name.hashCode ();
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
	    if (binding.name == name && binding.time_stamp < time_stamp)
	      {
		Object old = binding.get(); 
		env.remove(binding);
		return old;
	      }
	  }
	time_stamp = env.previous_time_stamp;
      }
  }

  public Object remove (Object name)
  {
    return remove((String) name);
  }

  public void remove (Binding binding)
  {
    int hash = binding.name.hashCode ();
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
	    if (b.time_stamp + 1 == time_stamp)
	      time_stamp--;
	    num_bindings--;
	    return;
	  }
	prev = b;
	b = next;
      }
  }

  public Object get (String name)
  {
    Binding binding = lookup (name);
    return binding == null ? null : binding.get ();
  }

  public Object get (Object name)
  {
    Binding binding = lookup ((String) name);
    return binding == null ? null : binding.get ();
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
    return "#<environment "
      + (name != null ? name : super.toString ())
      + '>';
  }

  /**
   * Evaluate an expression in this Environment.
   */
  /*
  final public Object
  eval (Expression expr)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    return expr.eval (this);
  }
  */
}
