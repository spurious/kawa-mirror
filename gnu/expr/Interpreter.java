package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.CodeAttr;

// WARNING many of the fields/method will be to Scheme instead.

/**
 * Contains various language-dependent methods.
 * Also contains "global" state about the executation environment,
 * such as the global Environment.  There can be multiple Interpreters
 * associated with different threads, representing mutiple top-levels.
 * (However, this functionality is incomplete.)
 */

public abstract class Interpreter
{
  public static Interpreter defaultInterpreter = null;

  public static Interpreter getInterpreter() { return defaultInterpreter; }

  // These will be moved from here.  FIXME
  static public final Boolean  trueObject = Boolean.TRUE;
  static public final Boolean  falseObject = Boolean.FALSE;

  static public final Undefined undefinedObject = new Undefined().getInstance();
  static public final Object voidObject = Values.empty;

  static public final String quote_sym = "quote";
  static public final String unquote_sym = "unquote";
  static public final String unquotesplicing_sym = "unquote-splicing";
  static public final String quasiquote_sym = "quasiquote";

  /** Test if a value is considered "true" in this language. */
  public boolean isTrue(Object value)
  {
    return value != Boolean.FALSE;
  }

  public Object booleanObject(boolean b)
  {
    return b ? Boolean.TRUE : Boolean.FALSE;
  }

  /** The value to return for a "void" result. */
  public Object noValue()
  {
    return Values.empty;
  }

  /** True if functions are in a separate anme space from variable.
   * Is true for e.g. Common Lisp, Emacs Lisp;  false for Scheme. */
  public boolean hasSeparateFunctionNamespace()
  {
    return false;
  }

  protected Environment environ;

  public Environment getEnvironment() { return environ; }
  public void setEnvironment(Environment environ) { this.environ = environ; }

  public void define(String sym, Object p)
  {
    environ.define (sym, p);
  }

  public Object lookup(String name)
  {
    return environ.get (name);
  }

  public abstract Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException;
  public abstract void print (Object obj, OutPort out);

  public Environment getNewEnvironment ()
  {
    return new Environment(environ);
  }

  public abstract String getName();

  public abstract gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages);

  public abstract gnu.bytecode.Type getTypeFor(Class clas);

  public void emitPushBoolean(boolean value, CodeAttr code)
  {
    code.emitGetStatic(value ? Compilation.trueConstant
		       : Compilation.falseConstant);
  }

  /** Generate code to test if an object is considered true.
   * Assume the object has been pushed on the JVM stack.
   * Generate code to push true or false as appropriate. */
  public void emitCoerceToBoolean(CodeAttr code)
  {
    emitPushBoolean(false, code);
    code.emitIfNEq();
    code.emitPushInt(1);
    code.emitElse();
    code.emitPushInt(0);
    code.emitFi();
  }

  public Object coerceFromObject(Class clas, Object obj)
  {
    return getTypeFor(clas).coerceFromObject(obj);
  }

  public Object coerceToObject(Class clas, Object obj)
  {
    return getTypeFor(clas).coerceToObject(obj);
  }

  public Object coerceToObject(int val)
  {
    return gnu.math.IntNum.make(val);
  }

  // The compiler finds registerEnvironment by using reflection.
  //
  // public static void registerEnvironment()
  // { Environment.setCurrent(new ...().getEnvironment()); }
}
