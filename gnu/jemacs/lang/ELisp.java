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

  protected void defun(String name, Object value)
  {
    gnu.commonlisp.lang.Symbols.setFunctionBinding(environ, name, value);
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
    Environment scmEnv = Scheme.builtin();
    environ = SymbolTable.make("interaction-environment."+(++elispCounter));
    Environment.setCurrent(environ);

    TRUE = environ.getSymbol("t");
    TRUE.set(TRUE);
    define("nil", FALSE);

    SymbolEnumeration e
      = Scheme.builtin().enumerateAllSymbols();
    while (e.hasMoreElements())
      {
	Symbol b = e.nextSymbol();
	Object val = b.get(null);
	if (val != null)
	  {
	    String name = b.getName();
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
	loadClass("kawa.lib.std_syntax");
	loadClass("kawa.lib.lists");
	loadClass("kawa.lib.strings");
	loadClass("gnu.commonlisp.lisp.PrimOps");
	loadClass("gnu.jemacs.lang.NumberOps");
	loadClass("gnu.jemacs.lang.MiscOps");
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	// Ignore - happens while building this directory.
      }

    defun(AddOp.$Pl); // "+"
    defun(AddOp.$Mn); // "-"
    defun(DivideOp.$Sl); // "/"
    defun(NumberCompare.$Eq);
    defun(NumberCompare.$Ls);
    defun(NumberCompare.$Gr);
    defun(NumberCompare.$Ls$Eq);
    defun(NumberCompare.$Gr$Eq);
    defun("self-insert-command", new gnu.jemacs.buffer.SelfInsertCommand());

    lambda lambda = new gnu.jemacs.lang.lambda();
    lambda.setKeywords(getSymbol("&optional"),
		       getSymbol("&rest"),
		       getSymbol("&key"));
    lambda.defaultDefault = nilExpr;
    defun("lambda", lambda);
    defun("defun", new gnu.commonlisp.lang.defun(lambda));
    defun("function", new gnu.commonlisp.lang.function(lambda));

    defun("defgroup", new defgroup());
    defun("defcustom", new defcustom());
    defun("defvar", new gnu.commonlisp.lang.defvar(false));
    defun("defconst", new gnu.commonlisp.lang.defvar(true));
    defun("defsubst", new gnu.commonlisp.lang.defun(lambda));
    defun("setq", new gnu.commonlisp.lang.setq());
    defun("prog1", gnu.commonlisp.lang.prog1.prog1);
    defun("prog2", gnu.commonlisp.lang.prog1.prog2);
    defun("progn", new kawa.standard.begin());
    defun("or", new kawa.standard.and_or(false, this));
    defun("and", new kawa.standard.and_or(true, this));
    defun("while", new gnu.jemacs.lang.While());
    defun("unwind-protect", new gnu.commonlisp.lang.UnwindProtect());
    defun("save-excursion", new gnu.jemacs.lang.SaveExcursion(false));
    defun("save-current-buffer", new gnu.jemacs.lang.SaveExcursion(true));
    defun("let", new kawa.standard.fluid_let(false, nilExpr));
    defun("let*", new kawa.standard.fluid_let(true, nilExpr));
    define_field ("concat", "kawa.lib.strings", "string$Mnappend");
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
	loadClass("gnu.jemacs.lisp.primitives");
	loadClass("gnu.jemacs.buffer.emacs");
	loadClass("gnu.jemacs.lisp.simple");
	loadClass("gnu.jemacs.lisp.autoloads");
	loadClass("gnu.jemacs.lisp.keymap");
	loadClass("gnu.jemacs.lisp.editfns");
	loadClass("gnu.jemacs.lisp.keydefs");
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
    Environment.setGlobal(interp.getEnvironment());
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
      name = "gnu.jemacs.buffer.Buffer";
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

  public void defineFromFieldValue(String name, Object part)
    throws Throwable
  {
    if (part instanceof Named)
      name = ((Named) part).getName();
    else
      name = name.intern();
    if (part instanceof Symbol)
      environ.addSymbol((Symbol) part);
    else if (part instanceof Procedure
	     || part instanceof kawa.lang.Syntax)
      gnu.commonlisp.lang.Symbols.setFunctionBinding(environ, name, part);
    else
      environ.define(name, part);
  }
  /* If non-final field:  FIXME
    Binding2 binding = new Binding2(name);
    setValue(binding, object);
    setConstraint(binding, new gnu.kawa.reflect.ClassMemberConstraint(field));
    environ.addBinding(binding);
  */

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
