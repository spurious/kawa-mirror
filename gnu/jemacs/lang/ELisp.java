package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.Char;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.commonlisp.lang.*;
import gnu.kawa.functions.DisplayFormat;

public class ELisp extends Lisp2
{
  static boolean charIsInt = false;

  /** Get a ELisp character object. */
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
    if (arg instanceof javax.swing.text.Position)
      return gnu.math.IntNum.make(1 + ((javax.swing.text.Position) arg).getOffset());
    return (gnu.math.Numeric) arg;
  }

  public static char asChar(Object x)
  {
    if (x instanceof Char)
      return ((Char) x).charValue();
    int i;
    if (x instanceof gnu.math.Numeric)
      i = ((gnu.math.Numeric) x).intValue();
    else if (x instanceof javax.swing.text.Position)
      i = ((javax.swing.text.Position) x).getOffset() + 1;
    else
      i = -1;
    if (i < 0 || i > 0xffff)
      throw new gnu.jemacs.buffer.Signal("error", "not a character value");
    return (char) i;
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
	throw new WrappedException(ex);
      }
  }

  protected void defun(String name, Object value)
  {
    gnu.commonlisp.lang.Symbol.setFunctionBinding(environ, name, value);
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
    environ = new SymbolTable();
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
	loadClass("gnu.commonlisp.lisp.PrimOps", environ);
	loadClass("gnu.jemacs.lang.NumberOps", environ);
	loadClass("gnu.jemacs.lang.MiscOps", environ);
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	// Ignore - happens while building this directory.
      }

    define("t", "t");
    define("nil", "nil");
    defun(AddOp.$Pl); // "+"
    defun(AddOp.$Mn); // "-"
    defun(DivideOp.$Sl); // "/"
    defun(NumberCompare.$Eq);
    defun(NumberCompare.$Ls);
    defun(NumberCompare.$Gr);
    defun(NumberCompare.$Ls$Eq);
    defun(NumberCompare.$Gr$Eq);

    lambda lambda = new gnu.jemacs.lang.lambda();
    lambda.setKeywords(getSymbol("&optional"),
		       getSymbol("&rest"),
		       getSymbol("&key"));
    lambda.defaultDefault = nilExpr;
    defun("lambda", lambda);
    defun("defun", new gnu.commonlisp.lang.defun(lambda));

    defun("defgroup", new defgroup());
    defun("defcustom", new defcustom());
    defun("defvar", new gnu.commonlisp.lang.defvar(false));
    defun("defconst", new gnu.commonlisp.lang.defvar(true));
    defun("defsubst", new gnu.commonlisp.lang.defun(lambda));
    defun("setq", new gnu.commonlisp.lang.setq());
    defun("progn", new kawa.standard.begin());
    defun("if", new kawa.standard.ifp());
    defun("or", new kawa.standard.and_or(false, this));
    defun("and", new kawa.standard.and_or(true, this));
    defun("while", new gnu.jemacs.lang.While());
    defun("unwind-protect", new gnu.commonlisp.lang.UnwindProtect());
    defun("save-excursion", new gnu.jemacs.lang.SaveExcursion(false));
    defun("save-current-buffer", new gnu.jemacs.lang.SaveExcursion(true));
    defun("let", new kawa.standard.fluid_let(false, nilExpr));
    defun("let*", new kawa.standard.fluid_let(true, nilExpr));
    defun("concat", new kawa.standard.string_append());
    Procedure not = new kawa.standard.not(this);
    defun("not", not);
    defun("null", not);
    defun("eq", new gnu.kawa.functions.IsEq(this, "eq"));
    defun("equal", new gnu.kawa.functions.IsEqual(this, "equal"));
    defun("typep", new gnu.kawa.reflect.InstanceOf(this));
    defun("princ", displayFormat);
    defun("prin1", writeFormat);
    try
      {
	loadClass("gnu.jemacs.lisp.primitives", environ);
	loadClass("gnu.jemacs.buffer.emacs", environ);
	loadClass("gnu.jemacs.lisp.simple", environ);
	loadClass("gnu.jemacs.lisp.autoloads", environ);
	loadClass("gnu.jemacs.lisp.keymap", environ);
	loadClass("gnu.jemacs.lisp.editfns", environ);
	loadClass("gnu.jemacs.lisp.keydefs", environ);
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	// Ignore - happens while building this directory.
      }
  }

  public static ELisp getInstance()
  {
    if (instance == null)
      {
        Environment saveEnv = Environment.getCurrent();
        try
          {
            instance = new ELisp();
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
    ELisp interp = new ELisp();
    Interpreter.defaultInterpreter = interp;
    Environment.setCurrent(interp.getEnvironment());
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return ELispReader.readObject(in);
  }

  static final DisplayFormat writeFormat = new Print(true);
  static final DisplayFormat displayFormat = new Print(false);

  public FormatToConsumer getFormat(boolean readable)
  {
    return readable ? writeFormat : displayFormat;
  }

  LangPrimType booleanType;

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
	      booleanType = new LangPrimType(Type.boolean_type, this);
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
		  gnu.commonlisp.lang.Symbol.setFunctionBinding(env, name, part);
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

  public static void readableChar(char ch, StringBuffer buf, boolean quote)
  {
    if (quote && (ch == '\\' || ch == '\'' || ch == '\"'))
      {
        buf.append('\\');
        buf.append(ch);
      }
    else if (ch > 127)
      {
        buf.append("\\u");
        String hex = Integer.toHexString(ch);
        for (int i = hex.length();  i < 4;  i++)  buf.append('0');
        buf.append(hex);
      }
    else if (ch >= ' ')
      buf.append(ch);
    else if (ch == '\t')  buf.append("\\t");
    else if (ch == '\r')  buf.append("\\r");
    else if (ch == '\n')  buf.append("\\n");
    else
      {
        buf.append("\\0");
        buf.append((ch >> 3) & 7);
        buf.append(ch & 7);
      }
  }

  /**
   * Call toString, quoting characters that are not ascii graphic chars.
   * This method will probably be moved somewhere more appropriate.
   */
  public static String readableString(Object obj)
  {
    String str = obj.toString();
    StringBuffer buf = new StringBuffer(200);
    for (int i = 0;  i < str.length();  i++)
      readableChar(str.charAt(i), buf, false);
    return buf.toString();
  }

  public static void main(String[] args)
  {
    registerEnvironment();
    if (args.length == 0)
      {
        args = new String[3];
        args[0] = "-e";
        args[1] = "(emacs)";
        args[2] = "--";
      }
    kawa.repl.main(args);
  }
}
