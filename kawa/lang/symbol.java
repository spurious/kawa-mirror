package kawa.lang;

import java.io.PrintStream;

public class symbol extends Object implements Printable
{
  private java.lang.String name;

  // Note:  No public constructor!
  private symbol(java.lang.String n)
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
   * Create or find a symbol with a given name.
   * @param name the print-name of the desired symbol
   * @return a symbol with the given name, newly created iff none such exist
   */
  static public symbol intern (String name)
  {
    symbol symbol = (symbol) symbolTable.get (name);
    if (symbol == null) {
      symbol = new symbol (name);
      symbolTable.put (name, symbol);
    }
    return symbol;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print(name);
  }
}
