package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.jemacs.buffer.BufferLocalConstraint;

/** Support for ELisp Symbols.
 * For now there are actually no "Symbol" objects.
 * Instead, at the ELisp level, the progrmmer uses java.lang.String objects.
 * This is for compatibility with Scheme, better integration with Java,
 * and because Strings are less "heighy-weight".  The special symbol `nil'
 * is actually the value gnu.lists.LList.Empty.
 * We use an Environment (ObArray) to map from from String to Binding2
 * objects (perhaps in the future we will use Symbol instances instead of
 * Binding2 instances).  The mapping from identifies to Binding2 objects
 * is does at class-initialization time, so no hash-table search is
 * needed for normal function or variable name lookup. */

public class Symbol extends Binding2
{
  public Symbol (String name)
  {
    super(name);
  }

  public static boolean isSymbol(Object val)
  {
    return val instanceof String || val == ELisp.FALSE
      || val instanceof Binding2;
  }

  public static boolean isBound(Object sym)
  {
    if (sym == ELisp.FALSE)
      return true;
    Binding binding = Environment.getCurrent().lookup((String) sym);
    return binding != null && binding.isBound();
  }
  
  public static Binding2 getBinding(Environment env, Object sym)
  {
    if (sym == ELisp.FALSE)
      sym = "nil";
    return Binding2.getBinding2(env, (String) sym);
  }

  public static Binding2 getBinding(Object sym)
  {
    if (sym == ELisp.FALSE)
      sym = "nil";
    return Binding2.getBinding2(Environment.getCurrent(), (String) sym);
  }

  public static void setValueBinding(Object symbol, Object value)
  {
    getBinding(symbol).set(value);
  }

  public static Object getPrintName(String sym)
  {
    return ELisp.getString(sym);
  }

  public static Object getPrintName(Object sym)
  {
    return sym == ELisp.FALSE ? "nil" : ELisp.getString((String) sym);
  }

  public static Object getFunctionBinding (Object symbol)
  {
    Binding2 binding = getBinding(symbol);
    return binding.functionValue;
  }

  public static Object getFunctionBinding (Environment environ, Object symbol)
  {
    Binding2 binding = getBinding(environ, symbol);
    return binding.functionValue;
  }

  public static void setFunctionBinding (Environment environ,
					 Object symbol, Object value)
  {
    Binding2 binding = getBinding(environ, symbol);
    binding.functionValue = value;
  }

  /**
   * The mapping from symbols (Java Strings) to property lists.
   * The traditional implementation of property lists is that each Symbol
   * contains a field pointing to the property list.  We prefer to use
   * Java Strings to implement Symbols, so there is no place for the
   * property list field.  Instead, we use this global table to map
   * from a symbol to the symbol's property list.
   */
  static Environment propertyLists = new Environment(100);

  /**
   * Get the property list of a symbol.
   */
  public static Object getPropertyList(Object symbol)
  {
    String name = symbol == ELisp.FALSE ? "nil" : (String) symbol;
    Binding binding = propertyLists.lookup(name);
    if (binding == null)
      return ELisp.FALSE;
    return binding.isBound() ? binding.get() : ELisp.FALSE;
  }

  /**
   * Set the property list of a symbol.
   */
  public static void setPropertyList(Object symbol, Object plist)
  {
    String name = symbol == ELisp.FALSE ? "nil" : (String) symbol;
    propertyLists.defineValue(name, plist);
  }

  /**
   * Given a property list and a key, find the corresponing property value.
   */
  public static Object plistGet(Object plist, Object prop, Object dfault)
  {
    while (plist instanceof Pair)
      {
	Pair pair = (Pair) plist;
	if (pair.car == prop)
	  return ((Pair) pair.cdr).car;
      }
    return dfault;
  }

  public static Object plistPut(Object plist, Object prop, Object value)
  {
    for (Object p = plist; p instanceof Pair; )
      {
	Pair pair = (Pair) p;
	Pair next = (Pair) pair.cdr;
	if (pair.car == prop)
	  {
	    next.car = value;
	    return plist;
	  }
	p = next.cdr;
      }
    return new Pair(prop, new Pair(value, plist));
  }

  public static Object plistRemove(Object plist, Object prop)
  {
    Pair prev = null;
    for (Object p = plist; p instanceof Pair; )
      {
	Pair pair = (Pair) p;
	Pair next = (Pair) pair.cdr;
	p = next.cdr;
	if (pair.car == prop)
	  {
	    if (prev == null)
	      return p;
	    prev.cdr = p;
	    return plist;
	  }
	prev = next;
      }
    return plist;
  }


  /**
   * @param all true if make-variable-buffer-local,
   *  false if make-local-variable FIXME
   */
  public static void makeBufferLocal(Object symbol, boolean all)
  {
    BufferLocalConstraint.make(getBinding(symbol), all);
  }
}
