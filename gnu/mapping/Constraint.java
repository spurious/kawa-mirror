package gnu.mapping;

/** A Constraint is used to control the values in a Symbol.
 * This is a very general mechanism, since you can change the Constaint
 * associated with a Symbol as needed. */

public abstract class Constraint
{
  public abstract Object get (Symbol symbol, Object defaultValue);

  public final Object get (Symbol symbol)
  {
    Object value = get(symbol, Symbol.UNBOUND);
    if (value == Symbol.UNBOUND)
      throw new UnboundSymbol(symbol.getName());
    return value;
  }

  public abstract void set (Symbol symbol, Object value);

  public boolean isBound (Symbol symbol)
  {
    try
      {
        get(symbol);
        return true;
      }
    catch (UnboundSymbol ex)
      {
        return false;
      }
  }

  public Procedure getProcedure (Symbol symbol)
  {
    return (Procedure) get(symbol);
  }

  /** Return Environment containing a given Symbol.
   * Returns null if unknown. */
  public Environment getEnvironment (Symbol symbol)
  {
    return null;
  }

  protected final static Object getValue(Symbol symbol)
  {
    return symbol.value;
  }

  protected final static void setValue(Symbol symbol, Object value)
  {
    symbol.value = value;
  }

  protected final static Constraint getConstraint(Symbol symbol)
  {
    return symbol.constraint;
  }

  protected final static void setConstraint(Symbol symbol,
                                            Constraint constraint)
  {
    symbol.constraint = constraint;
  }

  /** Get value of "function binding" of a Symbol.
   * Some languages (including Common Lisp and Emacs Lisp) associate both
   * a value binding and a function binding with a symbol.
   * @return the function value, or Symbol.UNBOUND if no function binding.
   */
  public Object getFunctionValue(Symbol symbol)
  {
    return symbol.getProperty(Symbol.FUNCTION, Symbol.UNBOUND);
  }

  public void setFunctionValue(Symbol symbol, Object value)
  {
    if (value == Symbol.UNBOUND)
      symbol.removeProperty(Symbol.FUNCTION);
    else
      symbol.setProperty(Symbol.FUNCTION, value);
  }
}
