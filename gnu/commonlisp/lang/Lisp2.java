// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.CodeAttr;
import gnu.kawa.lispexpr.LispInterpreter;

/** Abstract class for Lisp-like languages with separate namespaces. */

public abstract class Lisp2 extends LispInterpreter
{
  public static final LList FALSE = LList.Empty;
  public static Symbol TRUE;
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

  public int getNamespaceOf(Declaration decl)
  {
    // This is a kludge because the hygiene renameing in SyntaxRules
    // (which is used for some macros that Lisp uses) doesn't distinguish
    // function and variable position.
    if (decl.isAlias())
      return FUNCTION_NAMESPACE+VALUE_NAMESPACE;
    return decl.isProcedureDecl() ? FUNCTION_NAMESPACE : VALUE_NAMESPACE;
  }

  public void defineFunction(String name, Object proc)
  {
    Environment.defineFunction(environ, name, proc);
  }

  /** Get a symbol for a given (interned) Java string. */
  public static Object getSymbol (String name)
  {
    if (name == "nil")
      return FALSE;
    return Environment.getCurrentSymbol(name);
    //return name;
  }

  /** Get a string for a given Java string. */
  public static Object getString (String name)
  {
    return new FString(name);
  }

  /** Get a string for a given symbol. */
  public static Object getString (Symbol symbol)
  {
    return getString(symbol.getName());
  }

  public Environment getNewEnvironment ()
  {
    return new SymbolTable(environ);
  }

}
