// Copyright (c) 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.CodeAttr;
import gnu.bytecode.ClassType;
import gnu.bytecode.Type;
import gnu.lists.*;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import gnu.kawa.reflect.*;
import java.io.*;
import java.lang.reflect.InvocationTargetException;

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

  /**
   * List of known languages and their Interpreter classes.
   * Each element is one or more language names, or filename extensions,
   * followed by the name of the Interpreter sub-class.
   * The table is searched from the beginning.
   */

  static String[][] languages =
  {
    { "scheme", ".scm", ".sc", "kawa.standard.Scheme" },
    { "krl", ".krl", "gnu.kawa.brl.BRL" },
    { "brl", ".brl", "gnu.kawa.brl.BRL" },
    { "emacs", "elisp", "emacs-lisp", ".el", "gnu.jemacs.lang.ELisp" },
    { "xquery", ".xql", "gnu.xquery.lang.XQuery" },
    { "q2", ".q2", "gnu.q2.lang.Q2" },
    { "xslt", "xsl", ".xsl", "gnu.kawa.xslt.XSLT" },
    { "commonlisp", "common-lisp", "clisp", "lisp",
      ".lisp", ".lsp", ".cl",
      "gnu.commonlisp.lang.CommonLisp" }
  };

  /** Get a list of all available languages */

  public static String[][] getLanguages()
  {
    return languages;
  }

  /** Add a language to the list.
   *
   * @param langMapping is a language definition, the first index
   *  is the language name, subsequent indexes are file types that
   *  might cause the language to be used and the final index is the
   *  name of the class that implements the language.
   */
  public static void registerLanguage(String[] langMapping)
  {
    String[][] newLangs = new String[languages.length + 1][];
    System.arraycopy(languages, 0, newLangs, 0, languages.length);
    newLangs[newLangs.length - 1] = langMapping;
    languages = newLangs;
  }

  public static Interpreter getInstanceFromFilenameExtension(String filename)
  {
    int dot = filename.lastIndexOf('.');
    if (dot > 0)
      {
	Interpreter interp = Interpreter.getInstance(filename.substring(dot));
	if (interp != null)
	  return interp;
      }
    return null;
  }

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
		    // In the future, we may support languages names that
		    // can be implemented by more than one Interpreter,
		    // so don't give up yet.
		    break;
		  }
		return getInstance(names[0], langClass);
	      }
	  }
      }
    return null;
  }

  protected Interpreter ()
  {
    gnu.lists.Convert.setInstance(KawaConvert.getInstance());
  }

  public static Interpreter getInstance (String langName, Class langClass)
  {
    try
      {
	java.lang.reflect.Method method;
	Class[] args = { };
	try
	  {
	    String capitalizedName
	      = (Character.toTitleCase(langName.charAt(0))
		 + langName.substring(1).toLowerCase());
	    String methodName = "get" + capitalizedName + "Instance";
	    method = langClass.getDeclaredMethod(methodName, args);
	  }
	catch (Exception ex)
	  {
	    method
	      = langClass.getDeclaredMethod("getInstance", args);
	  }
	return (Interpreter) method.invoke(null, Values.noArgs);
      }
    catch (Exception ex)
      {
	langName = langClass.getName();
	Throwable th;
	if (ex instanceof InvocationTargetException)
	  th = ((InvocationTargetException) ex).getTargetException();
	else
	  th = ex;
	// th.printStackTrace();
	throw new WrappedException("getInstance for '" + langName + "' failed",
				   th);
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

  /** Enter a value into the current environment. */
  public void define(String sym, Object p)
  {
    environ.define (sym, p);
  }

  /** Enter a function into the current environment.
   * The function is implemented using a static method.
   * @param name the language-level name of the function.
   * @param cname the fully-qualified name of the class containing the method.
   * @param mname the name of the static method.
   */
  protected void define_method(String name, String cname, String mname)
  {
    environ.define(name, ClassMethods.apply(cname, mname));
  }

  protected void define_field (String name, String cname, String fname)
  {
    StaticFieldConstraint.define(environ, name, cname, fname);
  }

  protected void define_field (String name, String cname)
  {
    StaticFieldConstraint.define(environ, name, cname,
				 Compilation.mangleNameIfNeeded(name));
  }

  /** Enter a named function into the current environment. */
  public final void defineFunction(Named proc)
  {
    defineFunction(proc.getName(), proc);
  }

  /** Enter a function into the current environment.
   * Same as define(name,proc) for Scheme, but not for (say) Common Lisp.
   **/
  public void defineFunction(String name, Object proc)
  {
    environ.define(name, proc);
  }

  /** Import all the public fields of an object. */
  public void defineAll(Object object)
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
		defineFromFieldValue(name, field.get(object));
	      }
	    catch (Throwable ex)
	      {
		throw new WrappedException("error accessing field "+field, ex);
	      }
	  }
	else
	  {
	    System.err.println("INTERNAL ERROR in defineAll for "
			       + name + " in " + clas);
	  }
      }
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
    else
      environ.define(name, part);
  }

  public void loadClass(String name)
    throws java.lang.ClassNotFoundException
  {
    try
      {
	Class clas = Class.forName(name);
	Object inst = clas.newInstance ();
	defineAll(inst);
	if (inst instanceof ModuleBody)
	  ((ModuleBody)inst).run();
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	throw ex;
      }
    catch (Exception ex)
      {
	throw new WrappedException("cannot load "+name, ex);
      }
  }

  public Object lookup(String name)
  {
    return environ.get (name);
  }

  public void print (Object obj, OutPort out)
  {
    print(obj, out, false);
  }

  public void print (Object value, OutPort out, boolean readable)
  {
    if (value == Values.empty)
      return;
    FormatToConsumer saveFormat = out.objectFormat;
    try
      {
	out.objectFormat = getFormat(readable);
	if (value instanceof Values)
	  {
	    Object[] values = ((Values) value).getValues();
	    for (int i = 0;  i < values.length;  i++)
	      out.println(values[i]);
	  }
	else
	  out.println(value);
      }
    finally
      {
	out.objectFormat = saveFormat;
      }
  }

  public abstract FormatToConsumer getFormat(boolean readable);

  public Consumer getOutputConsumer(Writer out)
  {
    OutPort oport = out instanceof OutPort ? (OutPort) out
      : new OutPort(out);
    oport.objectFormat = getFormat(false);
    return oport;
  }

  public Environment getNewEnvironment ()
  {
    return new Environment(environ);
  }

  public abstract String getName();

  public abstract Lexer getLexer(InPort inp, SourceMessages messages);

  /** Flag to tell parse that expression will be evaluated immediately.
   * I.e. we're not creating class files for future execution. */
  public static final int PARSE_IMMEDIATE = 1;
  /** Flag to tell parse to only read a single line if possible.
   * Multiple lines may be read if syntactically required. */
  public static final int PARSE_ONE_LINE = 2;

  /** Parse one or more expressions.
   * @param port the InPort to read the expressions from.
   * @param messages where to send error messages and warnings
   * @param options various flags, includding PARSE_IMMEDIATE 
   *   and PARSE_ONE_LINE
   * @return a new Compilation.
   *   May return null if PARSE_ONE_LINE on end-of-file.
   */
  public final Compilation parse(InPort port,
				    gnu.text.SourceMessages messages,
				    int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    Lexer lexer = getLexer(port, messages);
    return parse(lexer, options);
  }

  public abstract Compilation parse(Lexer lexer, int options)
    throws java.io.IOException, gnu.text.SyntaxException;

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
          return getTypeFor(spec.toString());
        if (spec instanceof Symbol)
          return getTypeFor(((Symbol) spec).getName());
        if (spec instanceof CharSeq)
          return ClassType.make(spec.toString());
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
        Declaration decl = Declaration.followAliases(rexp.getBinding());
        String name = rexp.getName();
        if (decl != null)
	  {
	    name = decl.getName();
	    exp = decl.getValue();
	    if (decl.isAlias()
		&& exp instanceof QuoteExp)
	      {
		Object val = ((QuoteExp) exp).getValue();
		if (val instanceof Location)
		  {
		    Location loc = (Location) val;
		    if (loc.isBound())
		      return asType(loc.get());
		    if (! (loc instanceof Named))
		      return null;
		    name = ((Named) loc).getName();
		  }
	      }
	    else if (! decl.getFlag(Declaration.IS_UNKNOWN))
	      return getTypeFor(exp);
	  }
	Object val = Environment.getCurrent().get(name);
	if (val instanceof Type)
	  return (Type) val;
        int len = name.length();
        if (len > 2 && name.charAt(0) == '<'
            && name.charAt(len-1) == '>')
          return getTypeFor(name.substring(1, len-1));
      }
    else if (exp instanceof ClassExp)
      {
	return ((ClassExp) exp).getType();
      }
    return null;
  }

  /** Used when defining a namespace alias (prefix), in the XML sense.
   * Define in a namespace prefix NS is equivalent to defining a constant
   * named NAMESPACE_PREFIX+"NS" whose value is the namespace URI. */
  public static final String NAMESPACE_PREFIX = "$Namespace$";

  public static final int VALUE_NAMESPACE = 1<<0;
  public static final int FUNCTION_NAMESPACE = 1<<1;
  public static final int NAMESPACE_PREFIX_NAMESPACE = 1<<2;

  /** Return the namespace (e.g value or function) of a Declaration.
   * Return a bitmask of all the namespces "covered" by the Declaration.
   */
  public int getNamespaceOf(Declaration decl)
  {
    return VALUE_NAMESPACE;
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

  public Procedure getPrompter()
  {
    Symbol pr = Environment.getCurrentSymbol("default-prompter");
    return pr == null ? null : pr.getProcedure();
  }

  /** Return the result of evaluating a string as a source expression. */
  public final Object eval (String string) throws Throwable
  {
    return eval(new CharArrayInPort(string));
  }

  /** Evaluate expression(s) read from a Reader.
   * This just calls eval(InPort).
   */
  public final Object eval (Reader in) throws Throwable
  {
    return eval(in instanceof InPort ? (InPort) in : new InPort(in));
  }

  /** Evaluate expression(s) read from an InPort. */
  public final Object eval (InPort port) throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    try
      {
	eval(port, ctx);
	return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
	throw ex;
      }
  }

  /** Evaluate a string and write the result value(s) on a Writer. */
  public final void eval (String string, Writer out) throws Throwable
  {
    eval(new CharArrayInPort(string), out);
  }

  /** Evaluate a string and write the result value(s) to a PrintConsumer.
   * This is to disambiguate calls using OutPort or XMLPrinter,
   * which are both Writer and Consumer. */
  public final void eval (String string, PrintConsumer out) throws Throwable
  {
    eval(string, getOutputConsumer(out));
  }

  /** Evaluate a string and write the result value(s) to a Consumer. */
  public final void eval (String string, Consumer out) throws Throwable
  {
    eval(new CharArrayInPort(string), out);
  }

  /** Read expressions from a Reader and write the result to a Writer. */
  public final void eval (Reader in, Writer out) throws Throwable
  {
    eval(in, getOutputConsumer(out));
  }

  /** Read expressions from a Reader and write the result to a Consumer. */
  public void eval (Reader in, Consumer out) throws Throwable
  {
    InPort port = in instanceof InPort ? (InPort) in : new InPort(in);
    CallContext ctx = CallContext.getInstance();
    Consumer save = ctx.consumer;
    try
      {
	ctx.consumer = out;
	eval(port, ctx);
      }
    finally
      {
	ctx.consumer = save;
      }
  }

  public void eval (InPort port, CallContext ctx) throws Throwable
  {
    SourceMessages messages = new SourceMessages();
    Environment saveEnviron = Environment.getCurrent();
    if (saveEnviron != environ)
      Environment.setCurrent(environ);
    Interpreter saveInterp = defaultInterpreter;
    Interpreter.defaultInterpreter = this;
    try
      {
	Compilation comp = parse(port, messages, PARSE_IMMEDIATE);
	ModuleExp.evalModule(environ, ctx, comp);
      }
    finally
      {
	if (saveEnviron != environ && saveEnviron != null)
	  Environment.setCurrent(saveEnviron);
	Interpreter.defaultInterpreter = saveInterp;
      }
    if (messages.seenErrors())
      throw new RuntimeException("invalid syntax in eval form:\n"
				 + messages.toString(20));
  }

  public Object getDefaultSymbolValue (Symbol sym, boolean function)
  {
    String uri = sym.getNamespaceURI();
    String name = sym.getName();
    if (function || ! hasSeparateFunctionNamespace())
      {
	if (uri != null && uri.startsWith("class:"))
	  return ClassMethods.apply(uri.substring(6), name);
      }
    // Last restort - check for unresolved namespace prefix.
    int colon = name.indexOf(':');
    Environment env = sym.getEnvironment();
    if (colon >= 0 & env != null)
      {
	String prefix = (NAMESPACE_PREFIX+name.substring(0, colon)).intern();
	Object uri_val = env.get(prefix, null);
	if (uri_val == null && colon == 0)
	  uri_val = "";
	if (uri_val != null)
	  {
	    sym = Symbol.make(uri_val, name.substring(colon+1));
	    return function ? getSymbolProcedure(sym)
	      : getSymbolValue(sym);
	  }
      }
    throw new UnboundSymbol(name);
  }

  public static Object getSymbolValue (Symbol sym)
  {
    Object val = sym.get(Symbol.UNBOUND);
    if (val == Symbol.UNBOUND)
      return getInterpreter().getDefaultSymbolValue(sym, false);
    return val;
  }

  public static Procedure getSymbolProcedure (Symbol sym)
  {
    Object val = sym.getFunctionValue(Symbol.UNBOUND);
    if (val == Symbol.UNBOUND)
      val = sym.get(Symbol.UNBOUND);
    if (val == Symbol.UNBOUND)
      val = getInterpreter().getDefaultSymbolValue(sym, true);
    return (Procedure) val;
  }

  // The compiler finds registerEnvironment by using reflection.
  //
  // public static void registerEnvironment()
  // { Environment.setGlobal(new ...().getEnvironment()); }
}
