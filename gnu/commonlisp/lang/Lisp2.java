// Copyright (c) 2001, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.CodeAttr;
import gnu.bytecode.ClassType;
import gnu.text.*;
import gnu.kawa.lispexpr.*;
import gnu.kawa.reflect.FieldLocation;

/** Abstract class for Lisp-like languages with separate namespaces. */

public abstract class Lisp2 extends LispLanguage
{
  public static final LList FALSE = LList.Empty;
  // FIXME - which namespace?
  public static final Symbol TRUE = Namespace.getDefault().getSymbol("t");
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
    return obj instanceof Keyword || obj == TRUE || obj == FALSE;
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

  protected void importLocation (Location loc)
  {
    Symbol name = ((NamedLocation) loc).getKeySymbol();
    if (environ.isBound(name, EnvironmentKey.FUNCTION))
      return;
    Object val;
    loc = loc.getBase();
    // Disable the following, for now, if using GCJ.  It hangs when using GCJ.
    // The problem appears to be with a _Jv_Field for a static field
    // that is in a BSS segment; the address in the _Jv_Field doesn't
    // get initialized.  FIXME.
    // (We do need to use this for JEmacs.  Sigh.)
    if (loc instanceof FieldLocation
        && ((FieldLocation) loc).isProcedureOrSyntax())
      {
        environ.addLocation(name, EnvironmentKey.FUNCTION, loc);
      }
    else if ((val = loc.get(null)) != null)
      {
        if (val instanceof Procedure || val instanceof kawa.lang.Syntax)
          defun(name, val);
        else if(val instanceof LangObjType)
          defun(name, ((LangObjType) val).getConstructor());
        else
          define(name.getName(), val);
      }
  }

  public ReadTable createReadTable ()
  {
    ReadTable tab = new Lisp2ReadTable();
    tab.initialize();
    tab.setInitialColonIsKeyword(true);
    return tab;
  }
}

class Lisp2ReadTable extends ReadTable
{
  protected Object makeSymbol (String name)
  {
    return Lisp2.asSymbol(name.intern());
  }
}
