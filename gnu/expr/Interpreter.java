package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.CodeAttr;
import gnu.bytecode.Type;

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

  /**
   * List of known languages and their Interpreter classes.
   * Each element is one or more language names, or filename extensions,
   * followed by the name of the Interpreter sub-class.
   * The table is searched from the beginning.
   */

  static String[][] languages =
  {
    { "scheme", ".scm", "kawa.standard.Scheme" },
    { "emacs", "elisp", "emacs-lisp", ".el", "gnu.jemacs.lang.ELisp" },
    { "commonlisp", "common-lisp", "clisp", "lisp", ".lisp", ".lsp",
      "gnu.commonlisp.lang.CommonLisp" }
  };

  /** Look for an interpreter for a language with the given name or extension.
   * If name is null, look for the first language available. */
  public static Interpreter getInstance (String name)
  {
    int langCount = languages.length;
    for (int i = 0;  i < langCount;  i++)
      {
	String[] names = languages[i];
	int nameCount = names.length - 1;
	for (int j = nameCount;  --j >= 0;  )
	  {
	    if (name == null || names[j].equalsIgnoreCase(name))
	      {
		Class langClass;
		try
		  {
		    langClass = Class.forName(names[nameCount]);
		  }
		catch (ClassNotFoundException ex)
		  {
		    // In the future, we may support langauges names that
		    // can be implemented by more than one Interpreter,
		    // so don't give up yet.
		    break;
		  }
		return getInstance(name, langClass);
	      }
	  }
      }
    return null;
  }

  public static Interpreter getInstance (String langName, Class langClass)
  {
    try
      {
	java.lang.reflect.Method method
	  = langClass.getDeclaredMethod("getInstance", new Class[0]);
	return (Interpreter) method.invoke(null, Values.noArgs);
      }
    catch (Exception ex)
      {
	if (langName == null)
	  langName = langClass.getName();
	throw new WrappedException("getInstance for '" + langName + "' failed",
				   ex);
      }
  }

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

  public abstract Type getTypeFor(Class clas);

  public static Type string2Type (String name)
  {
    Type t;
    if (name.endsWith("[]"))
      {
	t = string2Type(name.substring(0, name.length()-2));
	if (t == null)
	  return null;
	t = gnu.bytecode.ArrayType.make(t);
      }
    else if (gnu.bytecode.Type.isValidJavaTypeName(name))
      t = gnu.bytecode.Type.getType(name);
    else
      return null;
    return t;
  }

  public Type getTypeFor(String name)
  {
    return  string2Type(name);
  }

  /** "Coerce" a language-specific "type specifier" object to a Type. */
  public Type asType(Object spec)
  {
    if (! (spec instanceof Type))
      {
        if (spec instanceof Class)
          return getTypeFor((Class) spec);
        if (spec instanceof String)
          return getTypeFor((String) spec);
        if (spec instanceof gnu.kawa.util.AbstractString)
          return gnu.bytecode.ClassType.make(spec.toString());
      }
    return (Type) spec;
  }

  public Type getTypeFor(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        try
          {
            return asType(((QuoteExp) exp).getValue());
          }
        catch (Exception ex)
          {
            return null;
          }
      }
    else if (exp instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) exp;
        Declaration decl = rexp.getBinding();
        if (decl != null)
          return getTypeFor(decl.getValue());
        String name = rexp.getName();
        int len = name.length();
        if (len > 2 && name.charAt(0) == '<'
            && name.charAt(len-1) == '>')
          return getTypeFor(name.substring(1, len-1));
      }
    return null;
  }

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
