// Copyright (c) 2001, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.Char;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.kawa.functions.DisplayFormat;

public class CommonLisp extends Lisp2
{
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
      throw new ClassCastException("not a character value");
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

  static int lispCounter = 0;

  public CommonLisp()
  {
    Environment scmEnv = Scheme.builtin();
    environ = new SimpleEnvironment("interaction-environment."+(++lispCounter));
    Environment.setCurrent(environ);

    TRUE = environ.getSymbol("t");
    // TRUE.set(TRUE); FIXME
    define("t", TRUE);
    define("nil", FALSE);

    LocationEnumeration e = scmEnv.enumerateAllLocations();
    while (e.hasMoreElements())
      {
	Location loc = e.nextLocation();
	Object val = loc.get(null);
	if (val != null)
	  {
	    Symbol name = ((NamedLocation) loc).getKeySymbol();
	    if (val instanceof Procedure || val instanceof kawa.lang.Syntax)
	      defun(name, val);
	    else
	      define(name.getName(), val);
	  }
      }

    if (instance == null)
      instance = this;

    try
      {
	// Force it to be loaded now, so we can over-ride let* length etc.
	loadClass("kawa.lib.prim_syntax");
	loadClass("kawa.lib.std_syntax");
	loadClass("kawa.lib.lists");
	loadClass("kawa.lib.strings");
	loadClass("gnu.commonlisp.lisp.PrimOps");
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	// Ignore - happens while building this directory.
      }

    kawa.lang.Lambda lambda = new kawa.lang.Lambda();
    lambda.setKeywords(asSymbol("&optional"),
		       asSymbol("&rest"),
		       asSymbol("&key"));
    lambda.defaultDefault = nilExpr;
    defun("lambda", lambda);
    defun("defun", new defun(lambda));

    defun("defvar", new defvar(false));
    defun("defconst", new defvar(true));
    defun("defsubst", new defun(lambda));
    defun("function", new function(lambda));
    defun("setq", new setq());
    defun("prog1", new prog1("prog1", 1));
    defun("prog2", prog1.prog2);
    defun("progn", new kawa.standard.begin());
    defun("unwind-protect", new gnu.commonlisp.lang.UnwindProtect());
    Procedure not = new kawa.standard.not(this);
    defun("not", not);
    defun("null", not);
    defun("eq", new gnu.kawa.functions.IsEq(this, "eq"));
    defun("equal", new gnu.kawa.functions.IsEqual(this, "equal"));
    defun("typep", new gnu.kawa.reflect.InstanceOf(this));
    defun("princ", displayFormat);
    defun("prin1", writeFormat);

    defProcStFld("functionp", "gnu.commonlisp.lisp.PrimOps");
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
    Environment.setGlobal(interp.getEnvironment());
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return CLispReader.readObject(in);
  }

  static final DisplayFormat writeFormat = new DisplayFormat(true, 'C');
  static final DisplayFormat displayFormat = new DisplayFormat(false, 'C');

  public FormatToConsumer getFormat(boolean readable)
  {
    return readable ? writeFormat : displayFormat;
  }

  LangPrimType booleanType;

  public Type getTypeFor(String name)
  {
    if (name == "t")
      name = "java.lang.Object";
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
	      booleanType = new LangPrimType(Type.boolean_type, this);
	    return booleanType;
	  }
	return Scheme.getNamedType(name);
      }
    return Type.make(clas);
  }
}


