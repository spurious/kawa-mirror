package kawa.lang;

import java.io.PrintStream;

public class Symbol extends Object implements Printable
{
  private java.lang.String name;

  // Note:  No public constructor!
  private Symbol (java.lang.String n)
  {
    name = new java.lang.String(n);
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
  static public Symbol intern (String name)
  {
    Symbol symbol = (Symbol) symbolTable.get (name);
    if (symbol == null) {
      symbol = new Symbol (name);
      symbolTable.put (name, symbol);
    }
    return symbol;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print(name);
  }
}
