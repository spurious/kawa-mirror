package gnu.commonlisp.lang;
import gnu.mapping.*;
import java.io.*;
import java.util.Hashtable;
import gnu.lists.FVector;

/** What Common Lisp calls a "package". */

public class SymbolTable extends Environment implements Externalizable
{
  static final Hashtable packageTable = new Hashtable(20);

  public static SymbolTable make(String name)
  {
    SymbolTable p = (SymbolTable) packageTable.get(name);
    if (p == null)
      {
	p = new SymbolTable();
	p.setName(name);
	packageTable.put(name, p);
      }
    return p;
  }

  public void registerPackage(String name)
  {
    packageTable.put(name, this);
  }

  public void registerPackage()
  {
    String name = getName();
    if (name != null)
      packageTable.put(name, this);
  }

  public void unregisterPackage()
  {
    String name = getName();
    if (name != null)
      packageTable.remove(name);
  }

  public void rename(String name)
  {
    unregisterPackage();
    setName(name);
    registerPackage();
  }

  /*
  String name;
  String[] nickNames;

  SymbolTable[] uses;
  SymbolTable[] usedBy;
  */

  /**
   * Get the function binding for a symbol.
   * @exception gnu.mapping.UnboundSymbol the name has no function binding
   */
  public Object getFunction(String name)
  {
    return Symbols.getFunctionBinding(this, name);
  }

  /** Set the function binding for a symbol.
   * this is equivalent to put.
   */
  public void putFunction(String name, Object value)
  {
    Symbols.setFunctionBinding(this, name, value);
  }

  /**
   * Define the value or function binding for a symbol, as appropriate
   */
  public Binding define (String name, Object value)
  {
    Binding binding = getBinding(name);
    if (value instanceof Procedure || value instanceof kawa.lang.Syntax)
      binding.setFunctionValue(value);
    else
      binding.set(value);
    return binding;
  }

  public static Environment asEnvironment(Object env)
  {
    if (env instanceof FVector)
      {
        // LispRef says the only valid way to create an obarray is
        // (make-vector LENGTH 0).  This here is a kludge to associate
        // an SymbolTable with a Vector.  It wastes memory (since we only
        // use element 0 as a link to the associated SymbolTable), but Emacs
        // does not create obarrays much, and the few places that create
        // them could be speeded up by using (make-obarray LENGTH).
        FVector vec = (FVector) env;
        env = vec.get(0);
        if (! (env instanceof Environment))
          {
            env = new SymbolTable(vec.size());
            vec.set(0, env);
          }
        
      }
    return (Environment) env;
  }

  public SymbolTable ()
  {
    super();
  }

  public SymbolTable (int initialCapacity)
  {
    super(initialCapacity);
  }

  public SymbolTable (Environment previous)
  {
    super(previous);
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
    if (name == null)
      return this;
    else
      {
	SymbolTable p = (SymbolTable) packageTable.get(name);
	if (p != null)
	  return p;
	registerPackage(name);
	return this;
      }
  }
}
