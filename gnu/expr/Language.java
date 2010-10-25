// Copyright (c) 2002, 2003, 2004, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.mapping.Location;
import gnu.lists.*;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import gnu.kawa.reflect.*;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import gnu.kawa.lispexpr.ClassNamespace; // FIXME

/**
 * Contains various language-dependent methods.
 * Also contains "global" state about the executation environment,
 * such as the global Environment.  There can be multiple Languages
 * associated with different threads, representing mutiple top-levels.
 * (However, this functionality is incomplete.)
 */

public abstract class Language
{
   protected static final InheritableThreadLocal<Language> current
   = new InheritableThreadLocal<Language>();
  protected static Language global;

  public static Language getDefaultLanguage()
  {
    Language lang = current.get();
    return lang != null ? lang : global;
  }

  static { Environment.setGlobal(BuiltinEnvironment.getInstance()); }

  public static void setCurrentLanguage (Language language)
  {
    current.set(language);
  }

  public static Language setSaveCurrent (Language language)
  {
    Language save = current.get();
    current.set(language);
    return save;
  }

  public static void restoreCurrent (Language saved)
  {
    current.set(saved);
  }

  /**
   * List of known languages and their Language classes.
   * Each element is one or more language names, or filename extensions,
   * followed by the name of the Language sub-class.
   * The table is searched from the beginning.
   */

  static String[][] languages =
  {
    { "scheme", ".scm", ".sc", "kawa.standard.Scheme" },
    { "krl", ".krl", "gnu.kawa.brl.BRL" },
    { "brl", ".brl", "gnu.kawa.brl.BRL" },
    { "emacs", "elisp", "emacs-lisp", ".el", "gnu.jemacs.lang.ELisp" },
    { "xquery", ".xquery", ".xq", ".xql", "gnu.xquery.lang.XQuery" },
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
   * @param langMapping is a language definition, the first element
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

  /** Detect the programming language of a file based on its first line.
   * @return a suitable Language or null if we didn't recognize one.
   */
  public static Language detect (InputStream in)
    throws IOException
  {
    if (! in.markSupported())
      return null;
    StringBuffer sbuf = new StringBuffer();
    in.mark(200);
    for (;;)
      {
        if (sbuf.length() >= 200)
          break;
        int c = in.read();
        if (c < 0 || c == '\n' || c == '\r')
          break;
        sbuf.append((char) c);
      }
    in.reset();
    return detect(sbuf.toString());
  }

  /** Detect the programming language of a file based on its first line.
   * @return a suitable Language or null if we didn't recognize one.
   */
  public static Language detect (InPort port)
    throws IOException
  {
    StringBuffer sbuf = new StringBuffer();
    port.mark(300);
    port.readLine(sbuf, 'P');
    port.reset();
    return detect(sbuf.toString());
  }

  /** Detect the programming language of a file based on its first line.
   * @param line the first input line
   * @return a suitable Language or null if we didn't recognize one.
   */
  public static Language detect (String line)
  {
    String str = line.trim();
    // Does the line contain the string "kawa:LANGUAGE" for a valid LANGUAGE?
    int k = str.indexOf("kawa:");
    if (k >= 0)
      {
        int i = k+5;
        int j = i;
        while (j < str.length()
               && Character.isJavaIdentifierPart(str.charAt(j)))
          j++;
        if (j > i)
          {
            String w = str.substring(i, j);
            Language lang = getInstance(w);
            if (lang != null)
              return lang;
          }
      }
    // Check for various Emacs language/mode patterns.
    if (str.indexOf("-*- scheme -*-") >= 0)
      return getInstance("scheme");
    if (str.indexOf("-*- xquery -*-") >= 0)
      return getInstance("xquery");
    if (str.indexOf("-*- emacs-lisp -*-") >= 0)
      return getInstance("elisp");
    if (str.indexOf("-*- common-lisp -*-") >= 0
        || str.indexOf("-*- lisp -*-") >= 0)
      return getInstance("common-lisp");
    // Does it start with an XQuery comment or XQuery version statement?
    if ((str.charAt(0) == '(' && str.charAt(1) == ':')
        || (str.length() >= 7 && str.substring(0, 7).equals("xquery ")))
      return getInstance("xquery");
    if (str.charAt(0) == ';' && str.charAt(1) == ';')
      return getInstance("scheme");
    return null;
  }

  public static Language getInstanceFromFilenameExtension(String filename)
  {
    int dot = filename.lastIndexOf('.');
    if (dot > 0)
      {
	Language lang = Language.getInstance(filename.substring(dot));
	if (lang != null)
	  return lang;
      }
    return null;
  }

  /** Look for a language with the given name or extension.
   * If name is null, look for the first language available. */
  public static Language getInstance (String name)
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
		    // can be implemented by more than one Language,
		    // so don't give up yet.
		    break;
		  }
		return getInstance(names[0], langClass);
	      }
	  }
      }
    return null;
  }

  protected Language ()
  {
    gnu.lists.Convert.setInstance(KawaConvert.getInstance());
  }

  public static Language getInstance (String langName, Class langClass)
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
	return (Language) method.invoke(null, Values.noArgs);
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

  /** The environment for language built-ins and predefined bindings. */
  protected Environment environ;

  /** If non-null, the user environment.
   * This allows "bunding" an Environment with a Language. This is partly to
   * match existing documentation, and partly for convenience from Java code.
   * Normally, userEnv is null, in which case the user environment is
   * extracted from the current thread. */
  protected Environment userEnv;

  /** Get current user environment. */
  public final Environment getEnvironment()
  {
    return userEnv != null ? userEnv : Environment.getCurrent();
  }

  static int envCounter;

  public final Environment getNewEnvironment ()
  {
    return Environment.make("environment-"+(++envCounter), environ);
  }

  public Environment getLangEnvironment() { return environ; }

  public NamedLocation lookupBuiltin (Symbol name, Object property, int hash)
  {
    return environ == null ? null : environ.lookup(name, property, hash);
  }

  /** Enter a value into the current environment. */
  public void define(String sym, Object p)
  {
    Symbol s = getSymbol(sym);
    environ.define(s, null, p);
  }

  /** Declare in the current Environment a variable aliased to a static field.
   */
  protected void defAliasStFld(String name, String cname, String fname)
  {
    StaticFieldLocation.define(environ, getSymbol(name), null, cname, fname);
  }

  /** Declare in the current Environment a procedure bound to a static field.
   * @param name the procedure's source-level name.
   * @param cname the name of the class containing the field.
   * @param fname the name of the field, which should be a static
   *   final field whose type extends gnu.mapping.Procedure.
   */
  protected void defProcStFld(String name, String cname, String fname)
  {
    Object property = (hasSeparateFunctionNamespace() ? EnvironmentKey.FUNCTION
		       : null);
    Symbol sym = getSymbol(name);
    StaticFieldLocation loc
      = StaticFieldLocation.define(environ, sym, property, cname, fname);
    loc.setProcedure();
  }

  /** Declare in the current Environment a procedure bound to a static field.
   * @param name the procedure's source-level name.
   * @param cname the name of the class containing the field.
   * The name of the field is the mangling of <code>name</code>.
   */
  protected void defProcStFld(String name, String cname)
  {
    defProcStFld(name, cname, Compilation.mangleNameIfNeeded(name));
  }

  /** Enter a named function into the current environment. */
  public final void defineFunction(Named proc)
  {
    Object name = proc.getSymbol();
    Symbol sym = (name instanceof Symbol ? (Symbol) name
		  : getSymbol(name.toString()));
    Object property = (hasSeparateFunctionNamespace() ? EnvironmentKey.FUNCTION
		       : null);
    environ.define(sym, property, proc);
  }

  /** Enter a function into the current environment.
   * Same as define(name,proc) for Scheme, but not for (say) Common Lisp.
   **/
  public void defineFunction(String name, Object proc)
  {
    Object property = (hasSeparateFunctionNamespace() ? EnvironmentKey.FUNCTION
		       : null);
    environ.define(getSymbol(name), property, proc);
  }

  public Object getEnvPropertyFor (java.lang.reflect.Field fld, Object value)
  {
    if (! hasSeparateFunctionNamespace())
      return null;
    if (Compilation.typeProcedure.getReflectClass()
	.isAssignableFrom(fld.getType()))
      return EnvironmentKey.FUNCTION;
    return null;
  }

  public Object getEnvPropertyFor (Declaration decl)
  {
    if (hasSeparateFunctionNamespace() && decl.isProcedureDecl())
      return EnvironmentKey.FUNCTION;
    return null;
  }

  public void loadClass(String name)
    throws java.lang.ClassNotFoundException
  {
    Class clas;
    try
      {
        clas = Class.forName(name);
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	throw ex;
      }
    try
      {
	Object inst = clas.newInstance ();
	ClassMemberLocation.defineAll(inst, this, Environment.getCurrent());
	if (inst instanceof ModuleBody)
	  ((ModuleBody)inst).run();
      }
    catch (Exception ex)
      {
	throw new WrappedException("cannot load "+name, ex);
      }
  }

  public Symbol getSymbol (String name)
  {
    return environ.getSymbol(name);
  }

  public Object lookup(String name)
  {
    return environ.get (name);
  }

  public AbstractFormat getFormat(boolean readable)
  {
    return null;
  }

  public Consumer getOutputConsumer(Writer out)
  {
    OutPort oport = out instanceof OutPort ? (OutPort) out
      : new OutPort(out);
    oport.objectFormat = getFormat(false);
    return oport;
  }

  public String getName()
  {
    String name = getClass().getName();
    int dot = name.lastIndexOf('.');
    if (dot >= 0)
      name = name.substring(dot+1);
    return name;
  }

  public abstract Lexer getLexer(InPort inp, SourceMessages messages);

  public Compilation getCompilation (Lexer lexer, SourceMessages messages, NameLookup lexical)
  {
    return new Compilation(this, messages, lexical);
  }

  /** Flag to tell parse that expression will be evaluated immediately.
   * I.e. we're not creating class files for future execution. */
  public static final int PARSE_IMMEDIATE = 1;
  /** Flag to tell parse to use current NameLookup.
   * As opposed to creating a new instance. */
  public static final int PARSE_CURRENT_NAMES = 2;
  /** Flag to tell parse to only read a single line if possible.
   * Multiple lines may be read if syntactically required. */
  public static final int PARSE_ONE_LINE = 4;
  /** Flag to tell parser to continue until we have the module name.
   * The parser is allowed to continue further, but must stop before
   * any module import. */
  public static final int PARSE_PROLOG = 8;
  public static final int PARSE_FOR_EVAL = PARSE_IMMEDIATE|PARSE_CURRENT_NAMES;
  public static final int PARSE_FOR_APPLET = 16;
  public static final int PARSE_FOR_SERVLET = 32;
  /** Compilation explicitly requested, not just because of an import. */
  public static final int PARSE_EXPLICIT = 64;

  public static boolean requirePedantic;

  /** Parse one or more expressions.
   * @param port the InPort to read the expressions from.
   * @param messages where to send error messages and warnings
   * @param options various flags, including PARSE_IMMEDIATE 
   *   and PARSE_ONE_LINE
   * @return a new Compilation.
   *   May return null if PARSE_ONE_LINE on end-of-file.
   */
  public final Compilation parse(InPort port,
                                 gnu.text.SourceMessages messages,
                                 int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return parse(getLexer(port, messages), options, null);
  }

  public final Compilation parse(InPort port,
                                 gnu.text.SourceMessages messages,
                                 ModuleInfo info)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return parse(getLexer(port, messages), Language.PARSE_PROLOG, info);
  }

  public final Compilation parse(InPort port,
                                 gnu.text.SourceMessages messages,
                                 int options, ModuleInfo info)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return parse(getLexer(port, messages), options, info);
  }

  public final Compilation parse(Lexer lexer, int options, ModuleInfo info)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    SourceMessages messages = lexer.getMessages();
    NameLookup lexical = ((options & PARSE_CURRENT_NAMES) != 0
                          ? NameLookup.getInstance(getEnvironment(), this)
                          : new NameLookup(this));
    boolean immediate = (options & PARSE_IMMEDIATE) != 0;
    Compilation tr = getCompilation(lexer, messages, lexical);
    if (requirePedantic)
      tr.pedantic = true;
    if (! immediate)
      tr.mustCompile = true;
    tr.immediate = immediate;
    tr.langOptions = options;
    if ((options & PARSE_EXPLICIT) != 0)
      tr.explicit = true;
    if ((options & PARSE_PROLOG) != 0)
      tr.setState(Compilation.PROLOG_PARSING);
    tr.pushNewModule(lexer);
    if (info != null)
      info.setCompilation(tr);
    if (! parse(tr, options))
      return null;
    if (tr.getState() == Compilation.PROLOG_PARSING)
      tr.setState(Compilation.PROLOG_PARSED);
    return tr;
  }

  public abstract boolean parse (Compilation comp, int options)
    throws java.io.IOException, gnu.text.SyntaxException;

  /** Perform any need post-processing after we've read all the modules
   * to be compiled.
   * Using a separate pass allows compiling mutually recursive modules. */
  public void resolve (Compilation comp)
  {
  }

  public Type getTypeFor(Class clas)
  {
    return Type.make(clas);
  }

  public final Type getLangTypeFor (Type type)
  {
    if (type.isExisting())
      {
        Class clas = type.getReflectClass();
        if (clas != null)
          return getTypeFor(clas);
      }
    return type;
  }

  public String formatType (Type type)
  {
    return type.getName();
  }

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

  public Type getTypeFor (String name)
  {
    return string2Type(name);
  }

  public final Type getTypeFor (Object spec, boolean lenient)
  {
    if (spec instanceof Type)
      return (Type) spec;
    if (spec instanceof Class)
      return getTypeFor((Class) spec);
    if (lenient
        && (spec instanceof FString
            || spec instanceof String
            || (spec instanceof Symbol && ((Symbol) spec).hasEmptyNamespace())
            || spec instanceof CharSeq))
      return getTypeFor(spec.toString());
    if (spec instanceof Namespace)
      {
        String uri = ((Namespace) spec).getName();
        if (uri != null && uri.startsWith("class:"))
          return getLangTypeFor(string2Type(uri.substring(6)));
      }
    return null;
  }

  /** "Coerce" a language-specific "type specifier" object to a Type. */
  public final Type asType(Object spec)
  {
    Type type = getTypeFor(spec, true);
    return type == null ? (Type) spec : type;
  }

  public final Type getTypeFor (Expression exp)
  {
    return getTypeFor(exp, true);
  }

  public Type getTypeFor (Expression exp, boolean lenient)
  {
    if (exp instanceof QuoteExp)
      {
        Object value = ((QuoteExp) exp).getValue();
        if (value instanceof Type)
          return (Type) value;
        if (value instanceof Class)
          return Type.make((Class) value);
        return getTypeFor(value, lenient);
      }
    else if (exp instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) exp;
        Declaration decl = Declaration.followAliases(rexp.getBinding());
        String name = rexp.getName();
        if (decl != null)
	  {
	    exp = decl.getValue();
            if (exp instanceof QuoteExp
                && decl.getFlag(Declaration.IS_CONSTANT)
                && ! decl.isIndirectBinding())
              {
		Object val = ((QuoteExp) exp).getValue();
                return getTypeFor(val, lenient);
              }
            else if (exp instanceof ClassExp || exp instanceof ModuleExp)
              {
                decl.setCanRead(true);
                return ((LambdaExp) exp).getClassType();
              }
	    else if (decl.isAlias()
		&& exp instanceof QuoteExp)
	      {
		Object val = ((QuoteExp) exp).getValue();
		if (val instanceof Location)
		  {
		    Location loc = (Location) val;
		    if (loc.isBound())
		      return getTypeFor(loc.get(), lenient);
		    if (! (loc instanceof Named))
		      return null;
		    name = ((Named) loc).getName();
		  }
	      }
	    else if (! decl.getFlag(Declaration.IS_UNKNOWN))
	      return getTypeFor(exp, lenient);
	  }
	Object val = getEnvironment().get(name);
	if (val instanceof Type)
	  return (Type) val;
        if (val instanceof ClassNamespace)
          return ((ClassNamespace) val).getClassType();
        int len = name.length();
        if (len > 2 && name.charAt(0) == '<'
            && name.charAt(len-1) == '>')
          return getTypeFor(name.substring(1, len-1));
      }
    else if (exp instanceof ClassExp || exp instanceof ModuleExp)
      {
	return ((LambdaExp) exp).getClassType();
      }
    return null;
  }

  public static/* for now */ Type unionType (Type t1, Type t2)
  {
    if (t1 == Type.toStringType)
      t1 = Type.javalangStringType;
    if (t2 == Type.toStringType)
      t2 = Type.javalangStringType;
    if (t1 == t2)
      return t1;
    if (t1 instanceof PrimType && t2 instanceof PrimType)
      {
        char sig1 = t1.getSignature().charAt(0);
        char sig2 = t2.getSignature().charAt(0);
        if (sig1 == sig2)
          return t1;
        if ((sig1 == 'B' || sig1 == 'S' || sig1 == 'I') && (sig2 == 'I' || sig2 == 'J'))
          return t2;
        if ((sig2 == 'B' || sig2 == 'S' || sig2 == 'I') && (sig1 == 'I' || sig1 == 'J'))
          return t1;
        if (sig1 == 'F' && sig2 == 'D')
          return t2;
        if (sig2 == 'F' && sig1 == 'D')
          return t1;
        return Type.objectType;
      }
    // FIXME handle class types better:
    // Type.lowestCommonSuperType(t1, t2);
    return Type.objectType;
  }

  public Declaration declFromField (ModuleExp mod, Object fvalue, Field fld)
  {
    String fname = fld.getName();
    Type ftype = fld.getType();
    boolean isAlias = ftype.isSubtype(Compilation.typeLocation);
    Object fdname;
    // FIXME if fvalue is FieldLocation, and field is final,
    // get name from value of field.
    boolean isImportedInstance;
    boolean externalAccess = false;
    boolean isFinal = (fld.getModifiers() & Access.FINAL) != 0;
    if ((isImportedInstance = fname.endsWith("$instance")))
      fdname = fname;
    else if (isFinal && fvalue instanceof Named) // && ! isAlias
      fdname = ((Named) fvalue).getSymbol();
    else
      {
	// FIXME move this to demangleName
	if (fname.startsWith(Declaration.PRIVATE_PREFIX))
          {
            externalAccess = true;
            fname = fname.substring(Declaration.PRIVATE_PREFIX.length());
          }
	fdname = Compilation.demangleName(fname, true).intern();
      }
    if (fdname instanceof String)
      {
        String uri = mod.getNamespaceUri();
        String sname = (String) fdname;
        Symbol sym;
        if (uri == null)
          fdname = SimpleSymbol.valueOf(sname);
        else
          fdname = Symbol.make(uri, sname);
      }
    Type dtype = isAlias ? Type.objectType
      : getTypeFor(ftype.getReflectClass());
    Declaration fdecl = mod.addDeclaration(fdname, dtype);
    boolean isStatic = (fld.getModifiers() & Access.STATIC) != 0;
    if (isAlias)
      {
        fdecl.setIndirectBinding(true);
        if (ftype instanceof ClassType
            && ((ClassType) ftype).isSubclass("gnu.mapping.ThreadLocation"))
          fdecl.setFlag(Declaration.IS_DYNAMIC);
      }
    else if (isFinal && ftype instanceof ClassType)
      {
        if (ftype.isSubtype(Compilation.typeProcedure))
          fdecl.setProcedureDecl(true);
        else if (((ClassType) ftype).isSubclass("gnu.mapping.Namespace"))
          fdecl.setFlag(Declaration.IS_NAMESPACE_PREFIX);
      }
    if (isStatic)
      fdecl.setFlag(Declaration.STATIC_SPECIFIED);
    fdecl.field = fld; 
    if (isFinal && ! isAlias) // FIXME? ok for location?
      fdecl.setFlag(Declaration.IS_CONSTANT);
    if (isImportedInstance)
      fdecl.setFlag(Declaration.MODULE_REFERENCE);
    fdecl.setSimple(false);
    if (externalAccess)
      fdecl.setFlag(Declaration.EXTERNAL_ACCESS|Declaration.PRIVATE);
    return fdecl;
  }

  public static final int VALUE_NAMESPACE = 1<<0;
  public static final int FUNCTION_NAMESPACE = 1<<1;
  public static final int NAMESPACE_PREFIX_NAMESPACE = 1<<2;

  /** Return the namespace (e.g value or function) of a Declaration.
   * Return a bitmask of all the namespaces "covered" by the Declaration.
   * Note this isn't a namespace in the XML sense; if a Declaration has
   * a specific namespace URI, then that is part of its symbol.
   * This namespace bitmap is a separate dimension, for the use of
   * languages that have separate namespaces for different kinds of
   * declarations, such as variables and functions.
   */
  public int getNamespaceOf(Declaration decl)
  {
    return VALUE_NAMESPACE;
  }

  /** True if a Declaration is in the specified namespace.
   * @param namespace normally a bitmask as returned by getNamespaceOf. */
  public boolean hasNamespace (Declaration decl, int namespace)
  {
    return (getNamespaceOf(decl) & namespace) != 0;
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

  public static synchronized void setDefaults (Language lang)
  {
    Language.setCurrentLanguage(lang);
    global = lang;
    // Assuming this is the initial (main) thread, make its Environment
    // the default (global) one, so child threads can inherit from it.
    // Thus command-line definitions etc get inherited.
    if (Environment.getGlobal() == BuiltinEnvironment.getInstance())
      Environment.setGlobal(Environment.getCurrent());
  }

  public Procedure getPrompter()
  {
    Object property = null;
    if (hasSeparateFunctionNamespace())
      property = EnvironmentKey.FUNCTION;
    Procedure prompter = (Procedure) getEnvironment()
      .get(getSymbol("default-prompter"), property, null);
    if (prompter != null)
      return prompter;
    else
      return new SimplePrompter();
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
    Language saveLang = Language.setSaveCurrent(this);
    try
      {
	Compilation comp = parse(port, messages, PARSE_FOR_EVAL);
	ModuleExp.evalModule(getEnvironment(), ctx, comp, null, null);
      }
    finally
      {
	Language.restoreCurrent(saveLang);
      }
    if (messages.seenErrors())
      throw new RuntimeException("invalid syntax in eval form:\n"
				 + messages.toString(20));
  }

  static protected int env_counter = 0;

  public void runAsApplication (String[] args)
  {
    setDefaults(this);
    kawa.repl.main(args);
  }
}

class SimplePrompter extends Procedure1
{
  public String prefix = "[";
  public String suffix = "] ";

  public Object apply1 (Object arg)
  {
    if (arg instanceof InPort)
      {
	InPort port = (InPort) arg;
	int line = port.getLineNumber() + 1;
	if (line >= 0)
	  return prefix + line + suffix;
      }
    return suffix;
  }

  // The compiler finds registerEnvironment by using reflection.
  //
  // public static void registerEnvironment()
  // { Environment.setGlobal(new ...().getEnvironment()); }
}
