package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.lists.*;

/** Support for Lisp Symbols.
 * The special symbol `nil' is actually the value gnu.lists.LList.Empty. */

public class Symbols
{
  private Symbols ()
  {
  }

  public static boolean isSymbol(Object val)
  {
    return val instanceof String || val == Lisp2.FALSE
      || val instanceof Symbol;
  }

  public static boolean isBound(Object sym)
  {
    if (sym == Lisp2.FALSE)
      return true;
    Symbol symbol = sym instanceof Symbol ? (Symbol) sym
      :Environment.getCurrent().lookup((String) sym);
    return symbol != null && symbol.isBound();
  }
  
  public static Symbol getSymbol(Environment env, Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return sym instanceof Symbol ? (Symbol) sym
      : env.getSymbol((String) sym);
  }

  public static Symbol getSymbol(Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return sym instanceof Symbol ? (Symbol) sym
      : Environment.getCurrent().getSymbol((String) sym);
  }

  public static void setValueBinding(Object symbol, Object value)
  {
    getSymbol(symbol).set(value);
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
    return getSymbol(symbol).getFunctionValue();
  }

  public static Object getFunctionBinding (Environment environ, Object symbol)
  {
    return getSymbol(environ, symbol).getFunctionValue();
  }

  public static void setFunctionBinding (Environment environ,
					 Object symbol, Object value)
  {
    getSymbol(environ, symbol).setFunctionValue(value);
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
