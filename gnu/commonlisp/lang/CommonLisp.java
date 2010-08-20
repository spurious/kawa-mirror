// Copyright (c) 2001, 2004, 2005  Per M.A. Bothner.
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
import gnu.kawa.functions.NumberCompare;

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

  public String getName()
  {
    return "CommonLisp";
  }

  // This field need to be public so that the findLiteral method in
  // gnu.expr.LitTable can find it.
  public static final CommonLisp instance;

  public static final Environment clispEnvironment
    = Environment.make("clisp-environment");

  public static final NumberCompare numEqu;
  public static final NumberCompare numGrt;
  public static final NumberCompare numGEq;
  public static final NumberCompare numLss;
  public static final NumberCompare numLEq;

  static
  {
    instance = new CommonLisp();

    instance.define("t", TRUE);
    instance.define("nil", FALSE);
    numEqu = NumberCompare.make(instance, "=",
                                NumberCompare.TRUE_IF_EQU);
    numGrt = NumberCompare.make(instance, ">",
                                NumberCompare.TRUE_IF_GRT);
    numGEq = NumberCompare.make(instance, ">=",
                                NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_EQU);
    numLss = NumberCompare.make(instance, "<",
                                NumberCompare.TRUE_IF_LSS);
    numLEq = NumberCompare.make(instance, "<=",
                                NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU);
    Environment saveEnv = Environment.setSaveCurrent(clispEnvironment);
    try
      {
        instance.initLisp();
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
      }
  }

  public CommonLisp()
  {
    environ = clispEnvironment;
  }

  void initLisp()
  {
    LocationEnumeration e = Scheme.builtin().enumerateAllLocations();
    while (e.hasMoreElements())
      {
        importLocation(e.nextLocation());
      }

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
    Procedure not = new gnu.kawa.functions.Not(this);
    defun("not", not);
    defun("null", not);
    defun("eq", new gnu.kawa.functions.IsEq(this, "eq"));
    defun("equal", new gnu.kawa.functions.IsEqual(this, "equal"));
    defun("typep", new gnu.kawa.reflect.InstanceOf(this));
    defun("princ", displayFormat);
    defun("prin1", writeFormat);

    defProcStFld("=", "gnu.commonlisp.lang.CommonLisp", "numEqu");
    defProcStFld("<", "gnu.commonlisp.lang.CommonLisp", "numLss");
    defProcStFld(">", "gnu.commonlisp.lang.CommonLisp", "numGrt");
    defProcStFld("<=", "gnu.commonlisp.lang.CommonLisp", "numLEq");
    defProcStFld(">=", "gnu.commonlisp.lang.CommonLisp", "numGEq");

    defProcStFld("functionp", "gnu.commonlisp.lisp.PrimOps");
  }

  public static CommonLisp getInstance()
  {
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(instance);
  }

  static final AbstractFormat writeFormat = new DisplayFormat(true, 'C');
  static final AbstractFormat displayFormat = new DisplayFormat(false, 'C');

  public AbstractFormat getFormat(boolean readable)
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
	      booleanType = new LangPrimType(Type.booleanType, this);
	    return booleanType;
	  }
	return Scheme.getNamedType(name);
      }
    return Type.make(clas);
  }
}
