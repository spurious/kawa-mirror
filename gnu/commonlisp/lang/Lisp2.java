// Copyright (c) 2001, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.CodeAttr;
import gnu.bytecode.ClassType;
import gnu.kawa.lispexpr.LispLanguage;

/** Abstract class for Lisp-like languages with separate namespaces. */

public abstract class Lisp2 extends LispLanguage
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
      code.emitGetStatic(ClassType.make("gnu.commonlisp.lang.Lisp2").getDeclaredField("TRUE"));
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

  public boolean selfEvaluatingSymbol (Object obj)
  {
    return obj instanceof Keyword || obj == TRUE | obj == FALSE;
  }

  public Object getEnvPropertyFor (java.lang.reflect.Field fld, Object value)
  {
    if (Compilation.typeProcedure.getReflectClass()
	.isAssignableFrom(fld.getType())
	|| value instanceof kawa.lang.Syntax)
      return EnvironmentKey.FUNCTION;
    return null;
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

  /** Get a symbol for a given (interned) Java string. */
  public static Object asSymbol (String name)
  {
    if (name == "nil")
      return FALSE;
    return Environment.getCurrent().getSymbol(name);
    //return name;
  }

  protected Symbol fromLangSymbol (Object obj)
  {
    if (obj == LList.Empty)
      return environ.getSymbol("nil");
    return super.fromLangSymbol(obj);
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

  protected void defun(String name, Object value)
  {
    environ.define(getSymbol(name), EnvironmentKey.FUNCTION, value);
    if (value instanceof Named)
      {
	Named n = (Named) value;
	if (n.getName() == null)
	  n.setName(name);
      }
  }

  protected void defun(Symbol sym, Object value)
  {
    environ.define(sym, EnvironmentKey.FUNCTION, value);
    if (value instanceof Procedure)
      {
	Procedure n = (Procedure) value;
	if (n.getSymbol() == null)
	  n.setSymbol(sym);
      }
  }

  private void defun(Procedure proc)
  {
    defun(proc.getName(), proc);
  }
}
