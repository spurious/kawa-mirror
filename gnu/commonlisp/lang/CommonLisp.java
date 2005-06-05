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
import gnu.kawa.util.AbstractFormat;
import gnu.kawa.lispexpr.ReadTable;

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

  static final CommonLisp instance;

  public static final Environment clispEnvironment
    = Environment.make("clisp-environment");

  static
  {
    instance = new CommonLisp();
    instance.environ = clispEnvironment;

    instance.define("t", TRUE);
    instance.define("nil", FALSE);
    CallContext ctx = CallContext.getInstance();
    Environment saveEnv = ctx.getEnvironmentRaw();
    try
      {
        ctx.setEnvironmentRaw(clispEnvironment);
        instance.initLisp();
      }
    finally
      {
        ctx.setEnvironmentRaw(saveEnv);
      }
  }

  public CommonLisp()
  {
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
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(instance);
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return CLispReader.readObject(in);
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
	      booleanType = new LangPrimType(Type.boolean_type, this);
	    return booleanType;
	  }
	return Scheme.getNamedType(name);
      }
    return Type.make(clas);
  }

  public ReadTable createReadTable ()
  {
    return ReadTable.getInitial();
  }
}
