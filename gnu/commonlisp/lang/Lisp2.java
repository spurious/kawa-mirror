// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.CodeAttr;

/** Abstract class for Lisp-like languages with separate namespaces. */

public abstract class Lisp2 extends Interpreter
{
  public static final LList FALSE = LList.Empty;
  public static Binding TRUE;
  public static final Expression nilExpr = new QuoteExp(FALSE);

  public boolean isTrue(Object value)
  {
    return value != FALSE;
  }

  public Object booleanObject(boolean b)
  {
    if (b) return TRUE; else return FALSE;
  }

  public void emitPushBoolean(boolean value, CodeAttr code)
  {
    if (value)
      code.emitPushString("t");
    else
      code.emitGetStatic(Compilation.scmListType.getDeclaredField("Empty"));
  }

  public Object noValue()
  {
    return FALSE;
  }

  public boolean hasSeparateFunctionNamespace()
  {
    return true;
  }

  /** Get a symbol for a given (interned) Java string. */
  public static Object getSymbol (String name)
  {
    if (name == "nil")
      return FALSE;
    return Environment.getCurrentBinding(name);
    //return name;
  }

  /** Get a string for a given Java string. */
  public static Object getString (String name)
  {
    return new FString(name);
  }

  /** Get a string for a given symbol. */
  public static Object getString (Binding symbol)
  {
    return getString(symbol.getName());
  }

  public Environment getNewEnvironment ()
  {
    return new SymbolTable(environ);
  }

}
