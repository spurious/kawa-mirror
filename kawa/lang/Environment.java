package kawa.lang;

/**
 * An environment contains (name->value) bindings.
 * @author	Per Bothner
 */

public class Environment // extends [somewhat] java.util.Dictionary
{
  Interpreter interp;

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

  public static Environment user ()
  {
    // transitional hack FIXME
    return Interpreter.curEnvironment ();
  }

  static Environment cur;

  public static Environment current ()
  {
    // FIXME - this should look at the current thread - somethink like:
    /*
      Thread thread = Thread.currentThread ();
      if (thread instanceof KawaThread)
      return ((KawaThread)thread).environment;
     */
    return cur == null ? user () : cur;
  }

  public Interpreter interpreter ()
  {
    return interp;
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
    this.interp = previous.interp;
  }

  // Obsolete (soon)
  public Environment (kawa.lang.Interpreter i)
  {
    this ();
    interp = i;
  }

  public Interpreter getInterpreter ()
  {
    return interp;
  }

  /**
   * Search for a variable binding by name.
   * @param sym the name of the binding to search for
   * @return the value of the binding, or null if not found
   */

  public Binding lookup (Symbol name)
  {
    int time_stamp = this.time_stamp;
    int hash = name.hashCode ();
    for (Environment env = this;  env != null;  env = env.previous)
      {
	Binding[] env_tab = env.table;
	int index = (hash & 0x7FFFFFFF) % env_tab.length;
//System.err.println ("lookup("+name +") in "+env+" index:"+index + " ts:"+time_stamp);
	for (Binding binding = env_tab[index];
	     binding != null;  binding = binding.chain)
	  {
 //System.err.println (" - binding:"+binding + " ts:"+binding.time_stamp);
	    if (binding.name == name && binding.time_stamp < time_stamp)
	      return binding;
	  }
	time_stamp = env.previous_time_stamp;
      }
    return null;
  }

  public final Binding define (String name, Object value)
  {
    return define (Symbol.make (name), value);
  }

  public Binding define (Symbol name, Object value)
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
//System.err.println ("define("+name+") in "+this +" index:"+index);
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

  public void remove (Binding binding)
  {
    int hash = binding.name.hashCode ();
    int index = (hash & 0x7FFFFFFF) % table.length;
    Binding prev = null;
    for (Binding b = table[index];  b != null ; )
      {
	if (b == binding)
	  {
	    if (prev == null)
	      table[index] = null;
	    else
	      prev.chain = b.chain;
	    if (b.time_stamp + 1 == time_stamp)
	      time_stamp--;
	    num_bindings--;
	    return;
	  }
	prev = b;
	b = b.chain;
      }
  }

  public Object get (Symbol name)
  {
    Binding binding = lookup (name);
    return binding == null ? null : binding.get ();
  }

  public Object get (Object name)
  {
    Binding binding = lookup ((Symbol) name);
    return binding == null ? null : binding.get ();
  }

  public Object put (Symbol name, Object value)
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
    return put ((Symbol) name, value);
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
