package kawa.lang;

import java.io.PrintStream;

public class Symbol extends Object implements Printable, Compilable
{
  private java.lang.String name;

  // Note:  No public constructor!
  private Symbol (java.lang.String n)
  {
    name = new java.lang.String(n);
  }

  public static final Symbol makeUninterned (String s)
  {
    return new Symbol (s);
  }

  private static int gensym_counter;

  /**
   * Generate a new un-interned Symbol with a unique name.
   * @return the new Symbol
   */
  public static final Symbol generate ()
  {
    return new Symbol ("GS." + Integer.toString(++gensym_counter));
  }

  private static java.util.Hashtable symbolTable = new java.util.Hashtable ();

  public int hashCode () { return name.hashCode (); }

  public final String toString()
  {
    return name;
  }

  /**
   * Create or find a Symbol with a given name.
   * @param name the print-name of the desired Symbol
   * @return a Symbol with the given name, newly created iff none such exist
   */
  static public Symbol make (String name)
  {
    Symbol symbol = (Symbol) symbolTable.get (name);
    if (symbol == null) {
      symbol = new Symbol (name);
      symbolTable.put (name, symbol);
    }
    return symbol;
  }

  static public final Symbol intern (String name)
  {
    return make (name);
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print(name);
  }

  public void emit (Literal literal, Compilation comp)
  {
    comp.method.compile_push_string (((Symbol)literal.value).toString ());
    comp.method.compile_invoke_static (comp.makeSymbolMethod);
    literal.flags |= Literal.ALLOCATED|Literal.INITIALIZED;
  }
}
