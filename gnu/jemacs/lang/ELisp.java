package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.expr.*;
import kawa.standard.Scheme;
import gnu.bytecode.Type;

public class ELisp extends Interpreter
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

  public boolean hasSeparateFunctionNamespace()
  {
    return true;
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

  static boolean charIsInt = false;

  /** Get a ELisp character object. */
  public static Object getCharacter(int c)
  {
    if (charIsInt)
      return gnu.math.IntNum.make(c);
    else
      return Char.make((char)c);
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
	defineAll(inst, env);
	if (inst instanceof gnu.expr.ModuleBody)
	  ((gnu.expr.ModuleBody)inst).run();
      }
    catch (Exception ex)
      {
	ex.printStackTrace(System.err);
	throw new WrappedException(ex);
      }
  }

  private void defun(String name, Object value)
  {
    Symbol.setFunctionBinding(environ, name, value);
    if (value instanceof Named)
      {
	Named n = (Named) value;
	if (n.getName() == null)
	  n.setName(name);
      }
  }

  private void defun(Procedure proc)
  {
    defun(proc.getName(), proc);
  }

  static int elispCounter = 0;

  public ELisp()
  {

    environ = new Environment();
    environ.setName ("interaction-environment."+(++elispCounter));
    Environment.setCurrent(environ);

    BindingEnumeration e
      = Scheme.getInstance().builtin().enumerateAllBindings();
    while (e.hasMoreElements())
      {
	Binding b = e.nextBinding();
	if (b.isBound())
	  {
	    String name = b.getName();
	    Object val = b.get();
	    if (val instanceof Procedure || val instanceof kawa.lang.Syntax)
	      defun(name, val);
	    else
	      define(name, val);
	  }
      }
    System.err.println("done");

    if (instance == null)
      instance = this;

    loadClass("gnu.jemacs.lang.SymbolOps", environ);
    loadClass("gnu.jemacs.lang.NumberOps", environ);
    loadClass("gnu.jemacs.lang.ArrayOps", environ);
    //    loadClass("kawa.lib.std_syntax", environ);
    define("t", "t");
    define("nil", "nil");
    defun(NumberCompare.makeLss("<"));
    defun(NumberCompare.makeLEq("<="));
    defun(NumberCompare.makeGrt(">"));
    defun(NumberCompare.makeGEq(">="));
    defun("lambda", new gnu.jemacs.lang.lambda());
    defun("defun", new gnu.jemacs.lang.defun());
    defun("setq", new gnu.jemacs.lang.setq());
    defun("if", new kawa.standard.ifp());
    defun("or", new kawa.standard.and_or(false, this));
    defun("while", new gnu.jemacs.lang.While());
    defun("let", new kawa.standard.fluid_let(false, nilExpr));
    defun("let*", new kawa.standard.fluid_let(true, nilExpr));
  }

  public static ELisp getInstance()
  {
    if (instance == null)
      instance = new ELisp();
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    ELisp interp = new ELisp();
    Interpreter.defaultInterpreter = interp;
    Environment.setCurrent(interp.getEnvironment());
  }

  public Environment getNewEnvironment ()
  {
    return new ObArray(environ);
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return ELispReader.readObject(in);
  }

  public void print (Object value, OutPort out)
  {
    if (value == Scheme.voidObject)
      return;
    if (value instanceof Values)
      {
	Object[] values = ((Values) value).getValues();
	for (int i = 0;  i < values.length;  i++)
	  {
	    SFormat.print (values[i], out);
	    out.println();
	  }
      }
    else
      {
	SFormat.print (value, out);
	out.println();
      }
    out.flush();
  }

  public Type getTypeFor (Class clas)
  {
    if (clas.isPrimitive())
      return Scheme.getNamedType(clas.getName());
    return Type.make(clas);
  }

  /** Import all the public fields of an object. */
  public static void defineAll(Object object, Environment env)
  {
    Class clas = object.getClass();
    java.lang.reflect.Field[] fields = clas.getFields();
    for (int i = fields.length;  --i >= 0; )
      {
	java.lang.reflect.Field field = fields[i];
	String name = field.getName();
	if ((field.getModifiers() & java.lang.reflect.Modifier.FINAL) != 0)
	  {
	    try
	      {
		Object part = field.get(object);
		if (part instanceof Named)
		  name = ((Named) part).getName();
		else if (part instanceof kawa.lang.Syntax) // FIXME
		  name = ((kawa.lang.Syntax) part).getName();
		else
		  name = name.intern();
		if (part instanceof Binding)
		  env.addBinding((Binding) part);
		else if (part instanceof Procedure
			 || part instanceof kawa.lang.Syntax)
		  Symbol.setFunctionBinding(env, name, part);
		else
		  env.define(name, part);
	      }
	    catch (Exception ex)
	      {
		throw new WrappedException("error accessing field "+field, ex);
	      }
	  }
	else
	  {
	    System.err.println("INTERNAL ERROR in ELisp.defineAll for "+name
+" in "+clas);
	    /*
	    Binding2 binding = new Binding2(name);
	    setValue(binding, object);
	    setConstraint(binding, new gnu.kawa.reflect.ClassMemberConstraint(field));
	    env.addBinding(binding);
	    */
	  }
      }
  }
}


