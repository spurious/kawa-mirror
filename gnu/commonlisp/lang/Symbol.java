package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.lists.*;

/** Support for Lisp Symbols.
 * For now there are actually no "Symbol" objects.
 * Instead, at the Lisp level, the progrmmer uses java.lang.String objects.
 * This is for compatibility with Scheme, better integration with Java,
 * and because Strings are less "heighy-weight".  The special symbol `nil'
 * is actually the value gnu.lists.LList.Empty.
 * We use an Environment (SymbolTable) to map from from String to Binding
 * objects (perhaps in the future we will use Symbol instances instead of
 * Binding instances).  The mapping from identifies to Binding objects
 * is does at class-initialization time, so no hash-table search is
 * needed for normal function or variable name lookup. */

public class Symbol extends Binding
{
  public Symbol (String name)
  {
    super(name);
  }

  public static boolean isSymbol(Object val)
  {
    return val instanceof String || val == Lisp2.FALSE
      || val instanceof Binding;
  }

  public static boolean isBound(Object sym)
  {
    if (sym == Lisp2.FALSE)
      return true;
    Binding binding = Environment.getCurrent().lookup((String) sym);
    return binding != null && binding.isBound();
  }
  
  public static Binding getBinding(Environment env, Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return env.getBinding((String) sym);
  }

  public static Binding getBinding(Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return Environment.getCurrent().getBinding((String) sym);
  }

  public static void setValueBinding(Object symbol, Object value)
  {
    getBinding(symbol).set(value);
  }

  public static Object getPrintName(String sym)
  {
    return Lisp2.getString(sym);
  }

  public static Object getPrintName(Object sym)
  {
    return sym == Lisp2.FALSE ? "nil" : Lisp2.getString((String) sym);
  }

  public static Object getFunctionBinding (Object symbol)
  {
    Binding binding = getBinding(symbol);
    return binding.getFunctionValue();
  }

  public static Object getFunctionBinding (Environment environ, Object symbol)
  {
    Binding binding = getBinding(environ, symbol);
    return binding.getFunctionValue();
  }

  public static void setFunctionBinding (Environment environ,
					 Object symbol, Object value)
  {
    Binding binding = getBinding(environ, symbol);
    binding.setFunctionValue(value);
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
    String name = symbol == Lisp2.FALSE ? "nil" : (String) symbol;
    Binding binding = propertyLists.lookup(name);
    if (binding == null)
      return Lisp2.FALSE;
    return binding.isBound() ? binding.get() : Lisp2.FALSE;
  }

  /**
   * Set the property list of a symbol.
   */
  public static void setPropertyList(Object symbol, Object plist)
  {
    String name = symbol == Lisp2.FALSE ? "nil" : (String) symbol;
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
}
