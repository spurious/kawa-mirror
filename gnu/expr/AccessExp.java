// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;

public abstract class AccessExp extends Expression
{
  /** The name of the variable to set - either a String or a Symbol. */
  Object symbol;
  /** If non-null, the local Declaration this refers to. */
  Declaration binding;
  public String string_name () { return symbol.toString(); }

  public final String getName()
  {
    return symbol instanceof Symbol ? ((Symbol) symbol).getName()
      : symbol.toString();
  }
  public final Object getSymbol() { return symbol; }
  /** If non-null, the local Declaration this refers to. */
  public final Declaration getBinding() { return binding; }

  public final void setBinding(Declaration decl) { binding = decl; }
}
