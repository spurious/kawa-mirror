package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.jemacs.lang.*;
import gnu.jemacs.lang.Symbol;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.Char;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.bytecode.CodeAttr;
import kawa.lang.SpecialType;

public class CommonLisp extends Interpreter
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
    // return Environment.getCurrentBinding(name);
    return name;
  }

  /** Get a string for a given Java string. */
  public static Object getString (String name)
  {
    return new FString(name);
  }

  /** Get a string for a given CommonLisp symbol. */
  public static Object getString (Binding symbol)
  {
    return getString(symbol.getName());
  }

  static boolean charIsInt = false;

  /** Get a CommonLisp character object. */
  public static Object getCharacter(int c)
  {
    if (charIsInt)
      return gnu.math.IntNum.make(c);
    else
      return Char.make((char)c);
  }

  public static gnu.math.Numeric asNumber(Object arg)
  {
    if (arg instanceof Char)
      return gnu.math.IntNum.make(((Char) arg).intValue());
    return (gnu.math.Numeric) arg;
  }

  public static char asChar(Object x)
  {
    if (x instanceof Char)
      return ((Char) x).charValue();
    int i;
    if (x instanceof gnu.math.Numeric)
      i = ((gnu.math.Numeric) x).intValue();
    else
      i = -1;
    if (i < 0 || i > 0xffff)
      throw new gnu.jemacs.buffer.Signal("error", "not a character value");
    return (char) i;
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new CLispReader(inp, messages);
  }

  public String getName()
  {
    return "CommonLisp";
  }

  static CommonLisp instance;

  public static void loadClass(String name, Environment env)
    throws java.lang.ClassNotFoundException
  {
    try
      {
	Class clas = Class.forName(name);
	Object inst = clas.newInstance ();
	defineAll(inst, env);
	if (inst instanceof gnu.expr.ModuleBody)
	  ((gnu.expr.ModuleBody)inst).run();
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	throw ex;
      }
    catch (Exception ex)
      {
        System.err.println("loadCklass:"+name);
        ex.printStackTrace(System.err);
	throw new WrappedException(ex);
      }
  }

  protected void defun(String name, Object value)
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

  public CommonLisp()
  {

    environ = new ObArray();
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

    if (instance == null)
      instance = this;

    try
      {
	// Force it to be loaded now, so we can over-ride let* length etc.
	loadClass("kawa.lib.std_syntax", environ);
	loadClass("kawa.lib.lists", environ);
	loadClass("kawa.lib.strings", environ);
	loadClass("gnu.jemacs.lang.SymbolOps", environ);
	loadClass("gnu.jemacs.lang.NumberOps", environ);
	loadClass("gnu.jemacs.lang.ArrayOps", environ);
	loadClass("gnu.jemacs.lang.StringOps", environ);
	loadClass("gnu.jemacs.lang.ListOps", environ);
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	// Ignore - happens while building this directory.
      }

    define("t", "t");
    define("nil", "nil");

    kawa.lang.Lambda lambda = new kawa.lang.Lambda();
    lambda.setKeywords("&optional", "&rest", "&key");
    lambda.defaultDefault = nilExpr;
    defun("lambda", lambda);
    defun("defun", new gnu.jemacs.lang.defun(lambda));

    defun("defvar", new defvar(false));
    defun("defconst", new defvar(true));
    defun("defsubst", new gnu.jemacs.lang.defun(lambda));
    defun("setq", new gnu.jemacs.lang.setq());
    defun("progn", new kawa.standard.begin());
    defun("if", new kawa.standard.ifp());
    defun("or", new kawa.standard.and_or(false, this));
    defun("and", new kawa.standard.and_or(true, this));
    defun("while", new gnu.jemacs.lang.While());
    defun("unwind-protect", new gnu.jemacs.lang.UnwindProtect());
    Procedure not = new kawa.standard.not(this);
    defun("not", not);
    defun("null", not);
    defun("eq", new gnu.kawa.functions.IsEq(this, "eq"));
    defun("equal", new gnu.kawa.functions.IsEqual(this, "equal"));
    defun("typep", new gnu.kawa.reflect.InstanceOf(this));
  }

  public static CommonLisp getInstance()
  {
    if (instance == null)
      {
        Environment saveEnv = Environment.getCurrent();
        try
          {
            instance = new CommonLisp();
          }
        finally
          {
            Environment.setCurrent(saveEnv);
          }
      }
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    CommonLisp interp = new CommonLisp();
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
    return CLispReader.readObject(in);
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

  SpecialType booleanType;

  public Type getTypeFor(String name)
  {
    if (name == "t")
      name = "java.lang.Object";
    else if (name == "marker")
      name = "gnu.jemacs.buffer.Marker";
    else if (name == "buffer")
      name = "gnu.jemacs.buffer.Bufffer";
    else if (name == "window")
      name = "gnu.jemacs.buffer.Window";
    return Scheme.string2Type(name);
  }

  public Type getTypeFor (Class clas)
  {
    if (clas.isPrimitive())
      {
	String name = clas.getName();
	if (name.equals("boolean"))
	  {
	    if (booleanType == null)
	      booleanType = new SpecialType(Type.boolean_type, this);
	    return booleanType;
	  }
	return Scheme.getNamedType(name);
      }
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
	    System.err.println("INTERNAL ERROR in CommonLisp.defineAll for "+name
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


