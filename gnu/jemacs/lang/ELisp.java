package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.expr.*;

public class ELisp extends kawa.standard.Scheme
{
  public static final LList FALSE = LList.Empty;
  public static final String TRUE = "t";
  public static final Expression nilExpr = new QuoteExp(FALSE);

  public boolean isTrue(Object value)
  {
    return value != FALSE;
  }

  public Object booleanObject(boolean b)
  {
    if (b) return TRUE; else return FALSE;
  }

  public Object noValue()
  {
    return FALSE;
  }

  /** Get a ELisp symbol for a given (interned) Java string. */
  public static Object getSymbol (String name)
  {
    if (name == "nil")
      return FALSE;
    // return Environment.getCurrentBinding(name);
    return name;
  }

  /** Get a ELisp string for a given Java string. */
  public static Object getString (String name)
  {
    return new FString(name);
  }

  /** Get a ELisp string for a given ELisp symbol. */
  public static Object getString (Binding symbol)
  {
    return getString(symbol.getName());
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new ELispReader(inp, messages);
  }

  public String getName()
  {
    return "Emacs-Lisp";
  }

  static ELisp instance;

  public static void loadClass(String name, Environment env)
  {
    try
      {
	Class clas = Class.forName(name);
	Object inst = clas.newInstance ();
	SymbolConstraint.defineAll(inst, env);
	if (inst instanceof gnu.expr.ModuleBody)
	  ((gnu.expr.ModuleBody)inst).run();
      }
    catch (Exception ex)
      {
	ex.printStackTrace(System.err);
	throw new WrappedException(ex);
      }
  }

  public ELisp()
  {
    Environment.setCurrent(environ);
    loadClass("gnu.jemacs.lang.SymbolOps", environ);
    SymbolConstraint.setFunctionBinding(environ, "lambda",
					new gnu.jemacs.lang.lambda());
    SymbolConstraint.setFunctionBinding(environ, "setq",
					new gnu.jemacs.lang.setq());
    SymbolConstraint.setFunctionBinding(environ, "or",
					new kawa.standard.and_or(false, this));
  }

  public static kawa.standard.Scheme getInstance() // Bad return type FIXME
  {
    if (instance == null)
      instance = new ELisp();
    return instance;
  }

  public Environment getNewEnvironment ()
  {
    if (kawaEnvironment == null)
      initScheme();
    return new ObArray(kawaEnvironment);
  }
}
