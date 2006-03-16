// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.Char;
import kawa.standard.Scheme;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.xquery.util.*;
import gnu.xml.*;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import java.io.Reader;
import java.util.Vector;
import gnu.kawa.functions.ConstantFunction0;
import gnu.kawa.reflect.ClassMethods;
import gnu.math.IntNum;

/** The XQuery language. */

public class XQuery extends Language
{
  public static final String XQUERY_FUNCTION_NAMESPACE
    = "http://www.w3.org/2004/10/xpath-functions";
  public static final String KAWA_FUNCTION_NAMESPACE
    = "http://kawa.gnu.org/";
  public static final String QEXO_FUNCTION_NAMESPACE
    = "http://qexo.gnu.org/";
  public static final String LOCAL_NAMESPACE
    = "http://www.w3.org/2004/10/xquery-local-functions";
  public static final String SCHEMA_NAMESPACE
    = "http://www.w3.org/2001/XMLSchema";
  public static final String XHTML_NAMESPACE
    = "http://www.w3.org/1999/xhtml";
  public static final Namespace xqueryFunctionNamespace
    = Namespace.getInstance(XQUERY_FUNCTION_NAMESPACE);
  public static final Namespace kawaFunctionNamespace
    = Namespace.getInstance(KAWA_FUNCTION_NAMESPACE);
  public static final Namespace qexoFunctionNamespace
    = Namespace.getInstance(QEXO_FUNCTION_NAMESPACE);
  public static final  Namespace[] defaultFunctionNamespacePath
    = { qexoFunctionNamespace,
	xqueryFunctionNamespace,
	Namespace.EmptyNamespace,
	kawaFunctionNamespace };
  static boolean charIsInt = false;

  /** Pseudo-namespace "prefix" for the default element namespace. */
  public static final String DEFAULT_ELEMENT_PREFIX = "elements$";
  /** Pseudo-namespace "prefix" for the default function namespace. */
  public static final String DEFAULT_FUNCTION_PREFIX = "functions$";

  Namespace defaultNamespace;

  public boolean hasSeparateFunctionNamespace()
  {
    return true;
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

  public boolean isTrue(Object value)
  {
    return gnu.xquery.util.BooleanValue.booleanValue(value);
  }

  public gnu.text.Lexer getLexer(InPort inp, SourceMessages messages)
  {
    return new XQParser(inp, messages, this);
  }

  /** Special parser flag used by <code>evalToFocusProc</code>. */
  public static final int PARSE_WITH_FOCUS = 0x10000;

  public Compilation parse(Lexer lexer, int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    XQParser parser = (XQParser) lexer;
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    Compilation tr = new Compilation(this, parser.getMessages(),
				     parser.lexical);
    tr.immediate = (options & PARSE_IMMEDIATE) != 0;
    XQResolveNames resolver = new XQResolveNames(tr);
    resolver.functionNamespacePath = parser.functionNamespacePath;
    ModuleExp mexp = tr.pushNewModule(lexer.getName());
    tr.mustCompileHere();
    ((XQParser) lexer).resolver = resolver;
    resolver.parser =  (XQParser) lexer;
    if ((options & PARSE_ONE_LINE) != 0)
      {
	Expression sexp = ((XQParser) lexer).parse(tr);
	if (sexp == null)
	  return null;
	mexp.body = sexp;
      }
    else if ((options & PARSE_WITH_FOCUS) != 0)
      {
	LambdaExp lexp = new LambdaExp(3);
	Declaration dotDecl = lexp.addDeclaration(XQParser.DOT_VARNAME);
	dotDecl.setFlag(Declaration.IS_SINGLE_VALUE);
	dotDecl.noteValue (null);  // Does not have a known value.
	lexp.addDeclaration(XQParser.POSITION_VARNAME, Type.int_type);
	lexp.addDeclaration(XQParser.LAST_VARNAME, Type.int_type);
	tr.push(lexp);
	lexp.body = ((XQParser) lexer).parse(tr);
	tr.pop(lexp);
	mexp.body = lexp;
      }
    else
      {
	Vector exps = new Vector(10);
	for (;;)
	  {
	    Expression sexp = ((XQParser) lexer).parse(tr);
	    if (sexp == null)
	      break;
	    exps.addElement(sexp);
	  }
	int nexps = exps.size();
	if (nexps == 0)
	  mexp.body = QuoteExp.voidExp;
	else if (nexps == 1)
	  mexp.body = (Expression) exps.elementAt(0);
	else
	  {
	    Expression[] arr = new Expression[nexps];
	    exps.copyInto(arr);
	    mexp.body = new BeginExp(arr);
	  }
      }
    tr.pop(mexp);
    
    if (false)
      {
	OutPort dout = OutPort.outDefault();
	dout.println ("[Before name-resolving \""+mexp.getName()+"\":");
	mexp.print(dout);
	dout.println(']');
	dout.flush();
      }

    resolver.resolveModule(mexp); // FIXME should move to resolve(Compilation)
    return tr;
  }

  public void resolve (Compilation comp)
  {
  }

  public int getNamespaceOf(Declaration decl)
  {
    return decl.isProcedureDecl() ? FUNCTION_NAMESPACE
      //: decl.isNamespaceDecl() ? NAMESPACE_PREFIX_NAMESPACE
      : VALUE_NAMESPACE;
  }

  public Symbol getSymbol (String name)
  {
    return Symbol.make(defaultNamespace, name);
  }

  public void define(String name, Object value)
  {
    Symbol sym = Symbol.make(defaultNamespace, name);
    Object prop = value instanceof Procedure ? EnvironmentKey.FUNCTION : null;
    environ.define(sym, prop, value);
  }

  protected void define_method(String name, String cname, String mname)
  {
    Symbol sym = Symbol.make(defaultNamespace, name);
    // This does require eager loading of the class, which takes
    // extra time on startup.  FIXME.
    ClassType ctype = ClassType.make(cname);
    Procedure proc = ClassMethods.apply(ctype, mname, '\0', this);
    proc.setSymbol(sym);
    environ.define(sym, EnvironmentKey.FUNCTION, proc);
  }

  public String getName()
  {
    return "XQuery";
  }

  static int envCounter = 0;

  /** Environment of pre-defined non-standard Qexo/Kawa functions. */
  public static Environment extensionsEnvEnv
    = Environment.getInstance(KAWA_FUNCTION_NAMESPACE);

  /** Call a procedure with a given focus (context).
   * @param proc a 3-operand <code>Procedure</code> as returned by
   *   <code>evalToFocusProc</code>
   * @param item the context item, passed as the first argument to <code>proc</code>
   * @param position the context position, passed as the second argument
   * @param size the context size, passed as the second argument
   * @param out where to send the result of <code>proc</code>
   */
  public void applyWithFocus (Procedure proc,
			      Object item, int position, int size,
			      Consumer out)
    throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    proc.check3(item, IntNum.make(position),IntNum.make(size), ctx);
    Consumer save = ctx.consumer;
    try
      {
	ctx.consumer = out;
	ctx.runUntilDone();
      }
    finally
      {
	ctx.consumer = save;
      }
  }

  /** Call a procedure with a given focus (context).
   * @param proc a 3-operand <code>Procedure</code> as returned by
   *   <code>evalToFocusProc</code>
   * @param item the context item, passed as the first argument to <code>proc</code>
   * @param position the context position, passed as the second argument
   * @param size the context size, passed as the second argument
   * @return the result of applying <code>proc</code>
   */
  public Object applyWithFocus (Procedure proc,
				Object item, int position, int size)
    throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    try
      {
	proc.check3(item, IntNum.make(position),IntNum.make(size), ctx);
	return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
	throw ex;
      }
  }

  /** Call a procedure with each item in a sequence as the context item.
   * @param proc a 3-operand <code>Procedure</code> as returned by
   *   <code>evalToFocusProc</code>
   * @param values a sequence.  The <code>proc</code> is called once for each
   *   item, with the item as the first argument, a 1-based index as the
   *   second argument, and the sequence size as the third argument.
   * @param out where to send the result of <code>proc</code>
   */
  public void applyWithFocus (Procedure proc, Object values, Consumer out)
    throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    Consumer save = ctx.consumer;
    try
      {
	ctx.consumer = out;
	applyWithFocus$X(proc, values, ctx);
      }
    finally
      {
	ctx.consumer = save;
      }
  }

  /** Call a procedure with each item in a sequence as the context item.
   * @param proc a 3-operand <code>Procedure</code> as returned by
   *   <code>evalToFocusProc</code>
   * @param values a sequence.  The <code>proc</code> is called once for each
   *   item, with the item as the first argument, a 1-based index as the
   *   second argument, and the sequence size as the third argument.
   * @return the result of applying <code>proc</code>
   */
  public Object applyWithFocus (Procedure proc, Object values)
    throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    try
      {
	applyWithFocus$X(proc, values, ctx);
	return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
	throw ex;
      }
  }

  /** Call a procedure with each item in a sequence as the context item.
   * @param proc a 3-operand <code>Procedure</code> as returned by
   *   <code>evalToFocusProc</code>
   * @param values a sequence.  The <code>proc</code> is called once for each
   *   item, with the item as the first argument, a 1-based index as the
   *   second argument, and the sequence size as the third argument.
   * @param ctx the <code>CallContext</code>.  The <code>$X</code> in the
   *   method name tells Kawa that this argument is implicit when invoked
   *   from XQuery.
   */
  public void applyWithFocus$X (Procedure proc, Object values, CallContext ctx)
    throws Throwable
  {
    if (values instanceof Values)
      {
	Values v = (Values) values;
	int count = v.size();
	if (count == 0)
	  return;
	int ipos = 0;
	IntNum size = IntNum.make(count);
	for (int i = 1;  ;  i++)
	  {
	    proc.check3(v.getPosNext(ipos), IntNum.make(i), size, ctx);
	    ctx.runUntilDone();
	    if (i == count)
	      break;
            ipos = v.nextPos(ipos);
	  }
      }
    else
      {
	IntNum one = IntNum.one();
	proc.check3(values, one, one, ctx);
	ctx.runUntilDone();
      }
  }

  /** Parse an XQuery expression that is the body of a procedure.
   * Helper method used by <code>evalWithFocus</code> methods.
   * @param expr an XQuery expression (query) to evaluate
   * @return a 3-operand Procedure whose arguments become
   * the context item, position, and size.
   */
  public Procedure evalToFocusProc (String expr)
    throws Throwable
  {
    SourceMessages messages = new SourceMessages();
    Procedure proc = evalToFocusProc(new CharArrayInPort(expr), messages);
    if (messages.seenErrors())
      throw new RuntimeException("invalid syntax in eval form:\n"
				 + messages.toString(20));
    return proc;
  }

  /** Parse an XQuery expression from a <code>Reader</code> that is the body of a procedure.
   * Helper method used by <code>evalWithFocus</code> methods.
   * @param in where we read the expression from
   * @param messages where to write syntax errors
   * @return a 3-operand Procedure whose arguments become
   * the context item, position, and size.
   */
  public Procedure evalToFocusProc (Reader in, SourceMessages messages)
    throws Throwable
  {
    InPort port = in instanceof InPort ? (InPort) in : new InPort(in);
    Compilation comp = parse(port, messages, PARSE_WITH_FOCUS|PARSE_IMMEDIATE);
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    try
      {
	ModuleExp.evalModule(Environment.getCurrent(), ctx, comp, null);
	return (Procedure) ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
	throw ex;
      }
  }

  /** Evaluate an expression with each item in a sequence as the context item.
   * @param in where we read the expression from
   * @param messages where to write syntax errors
   * @param values becomes the context sequence while
   *   evaluating <code>expr</code>.
   * @param out where to send the result of the expression
   */
  public void evalWithFocus (Reader in, SourceMessages messages,
			     Object values, Consumer out)
    throws Throwable
  {
    applyWithFocus(evalToFocusProc(in, messages), values, out);
  }

  /** Evaluate an expression with each item in a sequence as the context item.
   * @param expr an XQuery expression (query) to evaluate
   * @param values becomes the context sequence while
   *   evaluating the expression
   * @return the result of evaluating the expression
   */
  public Object evalWithFocus (String expr, Object values)
    throws Throwable
  {
    return applyWithFocus(evalToFocusProc(expr), values);
  }

  /** Evaluate an expression with a given focus (context).
   * @param expr an XQuery expression (query) to evaluate
   * @param item becomes the context item while evaluating <code>expr</code>.
   * @param position becomes the context position 
   * @param size becomes the context size
   * @return the result of evaluating <code>expr</code>
   */
  public Object evalWithFocus (String expr,
			       Object item, int position, int size)
    throws Throwable
  {
   return applyWithFocus(evalToFocusProc(expr), item, position, size);
  }

  /** Evaluate an expression with a given focus (context).
   * @param in where we read the expression from
   * @param messages where to write syntax errors
   * @param item becomes the context item while evaluating the expression
   * @param position becomes the context position 
   * @param size becomes the context size
   * @param out where to send the result of the expression
   */
  public void evalWithFocus (Reader in, SourceMessages messages,
			     Object item, int position, int size,
			     Consumer out)
    throws Throwable
  {
    applyWithFocus(evalToFocusProc(in, messages), item, position, size, out);
  }

  /** Evaluate an expression with a given focus (context).
   * Similar to <code>evalWithFocus(String, Object, Consumer)</code>.
   * The "$X" in the method name tells the Kawa compiler that the CallContext
   * argument is implicit, so it can be invoked from XQuery code thus:
   * <code>XQuery:eval-with-focus($xquery, "expr", $sequence)</code>
   */
  public void eval_with_focus$X (String expr,
				 Object values,
				 CallContext ctx)
    throws Throwable
  {
    applyWithFocus$X(evalToFocusProc(expr), values, ctx);
  }

  /** Evaluate an expression with a given focus (context).
   * Similar to <code>evalWithFocus(String, Object, int, int, Consumer)</code>.
   * The "$X" in the method name tells the Kawa compiler that the CallContext
   * argument is implicit, so it can be invoked from XQuery code thus:
   * <code>XQuery:eval-with-focus($xquery, "expr", $item, $pos, $size)</code>
   */
  public void eval_with_focus$X (String expr,
			       Object item, int position, int size,
			       CallContext ctx)
    throws Throwable
  {
    Procedure proc = evalToFocusProc(expr);
    proc.check3(item, IntNum.make(position), IntNum.make(size), ctx);
  }

  public static final Environment xqEnvironment
    = Environment.make(XQUERY_FUNCTION_NAMESPACE);

  static final XQuery instance = new XQuery();
  static { instance.initXQuery(); }

  public XQuery()
  {
    environ = xqEnvironment;
    defaultNamespace = xqueryFunctionNamespace;
  }

  private void initXQuery ()
  {
    ModuleBody.setMainPrintValues(true);

    /*
    Environment scmEnv = Scheme.builtin();

    Environment saveEnv = Environment.getCurrent();
    try
      {
	Environment.setCurrent(scmEnv);
	SymbolEnumeration e = scmEnv.enumerateAllSymbols();
	while (e.hasMoreElements())
	  {
	    Symbol b = e.nextSymbol();
	    Object val = b.get(null);
	    if (val instanceof Procedure)
	      extensionsEnvEnv.getSymbol(b.getName()).setFunctionValue(val);
	  }
	// Force it to be loaded now, so we can over-ride let* length etc.
	loadClass("kawa.lib.std_syntax");
	loadClass("kawa.lib.lists");
	loadClass("kawa.lib.strings");
	loadClass("gnu.commonlisp.lisp.PrimOps");
	loadClass("gnu.kawa.slib.XStrings");
      }
    catch (Throwable ex)
      {
	// Ignore.  We get a ClassNotFoundException if gnu.kawa.servlet.HTTP
	// was not built.  We get a NoClassDefFoundError if gnu.kawa.servlet.HTTP
	// can't find servlets in the classpath.
      }
    finally
      {
	Environment.setCurrent(saveEnv);
      }
    */

    defProcStFld("unescaped-data", "gnu.kawa.xml.MakeUnescapedData", "unescapedData");
    defProcStFld("item-at", "gnu.xquery.util.ItemAt", "itemAt");
    defProcStFld("count", "gnu.kawa.functions.CountValues", "countValues");
    defProcStFld("min", "gnu.xquery.util.MinMax", "min");
    defProcStFld("max", "gnu.xquery.util.MinMax", "max");
    defProcStFld("sum", "gnu.xquery.util.Reduce", "sum");
    defProcStFld("avg", "gnu.xquery.util.Average", "avg");
    defProcStFld("index-of", "gnu.xquery.util.IndexOf", "indexOf");
    defProcStFld("last-index-of", "gnu.xquery.util.LastIndexOf", "lastIndexOf");
    defProcStFld("sublist", "gnu.xquery.util.SubList", "subList"); // deprecated
    defProcStFld("subsequence", "gnu.xquery.util.SubList", "subList");
    define_method("empty", "gnu.xquery.util.SequenceUtils",
		  "isEmptySequence");
    define_method("exists", "gnu.xquery.util.SequenceUtils",
		  "exists");
    define_method("reverse", "gnu.xquery.util.SequenceUtils",
		  "reverse$X");
    defProcStFld("false", "gnu.xquery.lang.XQuery", "falseFunction");
    defProcStFld("true", "gnu.xquery.lang.XQuery", "trueFunction");
    defProcStFld("boolean", "gnu.xquery.util.BooleanValue", "booleanValue");
    defProcStFld("number", "gnu.xquery.util.NumberValue", "numberValue");
    defProcStFld("string-value", "gnu.xquery.util.StringValue", "stringValue");
    defProcStFld("string", "gnu.xquery.util.StringValue", "string");

    define_method("trace", "gnu.xquery.util.Debug", "trace");
    defProcStFld("write-to", "gnu.kawa.xml.WriteTo", "writeTo");
    defProcStFld("write-to-if-changed", "gnu.kawa.xml.WriteTo",
                 "writeToIfChanged");
    defProcStFld("iterator-items",
		 "gnu.kawa.xml.IteratorItems", "iteratorItems");
    defProcStFld("list-items", "gnu.kawa.xml.ListItems", "listItems");
    define_method("node-name", "gnu.kawa.xml.NodeName", "nodeName");
    define_method("lower-case", "gnu.xquery.util.StringValue", "lowerCase");
    define_method("upper-case", "gnu.xquery.util.StringValue", "upperCase");
    define_method("substring", "gnu.xquery.util.StringValue", "substring");
    define_method("string-length",
		  "gnu.xquery.util.StringValue", "stringLength");
    define_method("substring-before",
		  "gnu.xquery.util.StringValue", "substringBefore");
    define_method("substring-after",
		  "gnu.xquery.util.StringValue", "substringAfter");
    define_method("translate", "gnu.xquery.util.StringValue", "translate");
    define_method("string-pad", "gnu.xquery.util.StringValue", "stringPad");
    define_method("contains", "gnu.xquery.util.StringValue", "contains");
    define_method("starts-with", "gnu.xquery.util.StringValue", "startsWith");
    define_method("ends-with","gnu.xquery.util.StringValue", "endsWith");
    define_method("string-join", "gnu.xquery.util.StringValue", "stringJoin");
    define_method("concat", "gnu.xquery.util.StringValue", "concat$V");

    define_method("abs", "gnu.xquery.util.NumberValue", "abs");
    define_method("floor", "gnu.xquery.util.NumberValue", "floor");
    define_method("ceiling", "gnu.xquery.util.NumberValue", "ceiling");
    define_method("round-half-to-even", "gnu.xquery.util.NumberValue",
                  "roundHalfToEven");

    define_method("QName", "gnu.xquery.util.QNameUtils", "makeQName");
    define_method("prefix-from-QName", "gnu.xquery.util.QNameUtils",
		  "prefixFromQName");
    define_method("local-name-from-QName", "gnu.xquery.util.QNameUtils",
		  "localNameFromQName");
    define_method("namespace-uri-from-QName", "gnu.xquery.util.QNameUtils",
		  "namespaceURIFromQName");
    define_method("namespace-uri-for-prefix", "gnu.xquery.util.QNameUtils",
		  "namespaceURIForPrefix");
    define_method("in-scope-prefixes", "gnu.xquery.util.NodeUtils",
                  "inScopePrefixes$X");

    define_method("zero-or-one", "gnu.xquery.util.SequenceUtils", "zeroOrOne");
    define_method("one-or-more", "gnu.xquery.util.SequenceUtils", "oneOrMore");
    define_method("exactly-one", "gnu.xquery.util.SequenceUtils", "exactlyOne");

    defProcStFld("distinct-nodes", "gnu.kawa.xml.SortNodes", "sortNodes");

    // FIXME - should be imported?
    defProcStFld("children", "gnu.kawa.xml.Children", "children");
    defProcStFld("not", "kawa.standard.Scheme");

    defaultNamespace = qexoFunctionNamespace;
    defProcStFld("response-header", "gnu.kawa.servlet.HTTP");
    defProcStFld("response-content-type", "gnu.kawa.servlet.HTTP");
    defProcStFld("response-status", "gnu.kawa.servlet.HTTP");
    defProcStFld("error-response", "gnu.kawa.servlet.HTTP");
    defProcStFld("current-servlet", "gnu.kawa.servlet.HTTP");
    defProcStFld("current-servlet-context", "gnu.kawa.servlet.HTTP");
    defProcStFld("current-servlet-config", "gnu.kawa.servlet.HTTP");
    defProcStFld("servlet-context-realpath", "gnu.kawa.servlet.HTTP");
    defProcStFld("get-response", "gnu.kawa.servlet.HTTP");
    defProcStFld("get-request", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-method", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-uri", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-url", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-path-info", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-path-translated", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-servlet-path", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-query-string", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-parameter", "gnu.kawa.servlet.HTTP");
    defProcStFld("request-parameters", "gnu.kawa.servlet.HTTP");
    defaultNamespace = xqueryFunctionNamespace;
  }

  public static XQuery getInstance()
  {
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(new XQuery());
  }

  public static final ConstantFunction0 falseFunction
    = new ConstantFunction0("false", Boolean.FALSE);
  public static final ConstantFunction0 trueFunction
    = new ConstantFunction0("true", Boolean.TRUE);

  public static final XMLFormat writeFormat = new XMLFormat();

  public AbstractFormat getFormat(boolean readable)
  {
    return writeFormat;
  }

  public Consumer getOutputConsumer(java.io.Writer out)
  {
    return new XMLPrinter(out, false);
  }

  LangPrimType booleanType;

  static Object[] typeMap =
    { "string", Type.string_type,
      "boolean", Type.boolean_type,
      "QName", "gnu.xml.SName",
      "integer", "gnu.math.IntNum",
      "positiveInteger", "gnu.math.IntNum",
      "nonPositiveInteger", "gnu.math.IntNum",
      "negativeInteger", "gnu.math.IntNum",
      "nonNegativeInteger", "gnu.math.IntNum",
      /* #ifdef use:java.net.URI */
      "anyURI", "java.net.URI",
      /* #else */
      // "anyURI", Type.string_type,
      /* #endif */
      "decimal", "gnu.math.RealNum"
    };

  public Type getTypeFor(String name)
  {
    if (name == "t")
      name = "java.lang.Object";
    String core = name.startsWith("xs:") ? name.substring(3) : name;
    for (int i = typeMap.length;  (i -= 2) >= 0; )
      {
	if (typeMap[i].equals(core))
	  {
	    Object t = typeMap[i+1];
	    if (t instanceof String)
	      return Scheme.string2Type((String) t);
	    else
	      return (Type) t;
	  }
      }
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

  public Procedure getPrompter()
  {
    return new Prompter();
  }

  /*
  static boolean isPunctuation (char ch)
  {
    return ch == '-' || ch == '.' || ch == ':' || ch == '_'
      || (ch >= 0xB7 // To short-circuit rare tests
	  && (ch == '\u00B7' // middle dot
	      || ch == '\u0387' // greek ano teleia
	      || ch == '\u06dd' // arabic end of ayah
	      || ch == '\u06de' // arabic start of rub el hizb
	      ));
  }

  static boolean isMark (char ch)
  {
    return ! Character.isLetter(ch)
      && ! Characfter.isDigit(ch)
      && Character.isJavaIdnteiferiPart(ch);
  }
  */

  /** Mangle an XML name as specified by JAXB. */
  static void mangle (String name, int start, int length,
		      StringBuffer sbuf, char mode)
  {
    // One of 'P' for punctuation; 'D' for digit;  'M' for mark;
    // 'L' for lower-case; 'U' for upper-case; 'O' other (uncased) letter.
    char prev = 'P';
    int outStart = sbuf.length();
    for (int i = 0;  i < length;  )
      {
	boolean wordStart;
	char ch = name.charAt(start + i);
	i++;
	if (Character.isUpperCase(ch))
	  {
	    wordStart = prev != 'U'
	      || (i < length
		  && Character.isLowerCase(name.charAt(start+i)));
	    prev = 'U';
	  }
	else if (Character.isLowerCase(ch))
	  {
	    wordStart = prev != 'L' || prev != 'U';
	    prev = 'L';
	  }
	else if (Character.isLetter(ch))
	  { // uncased letter
	    wordStart = prev != 'O';
	    prev = 'O';
	  }
	else if (Character.isDigit(ch))
	  {
	    wordStart = prev != 'D';
	    prev = 'D';
	  }
	else if (Character.isJavaIdentifierPart(ch))
	  {
	    wordStart = prev != 'D' && prev != 'M';
	    prev = 'M';
	  }
	else // if (isPunctuation(ch))
	  {
	    prev = 'P';
	    continue;
	  }
	if (wordStart || mode == '_')
	  {
	    if (wordStart && mode == '_' && sbuf.length() > outStart)
	      sbuf.append('_');
	    ch = Character.toUpperCase(ch);
	  }
	sbuf.append(ch);
      }
  }
  public static String mangle (String name)
  {
    StringBuffer sbuf = new StringBuffer();
    mangle(name, 0, name.length(), sbuf, 'U');
    return sbuf.toString();
  }

  public static Object getExternal (SName name, Object type)
  {
    Environment env = Environment.getCurrent();
    Symbol symbol = name.getSymbol();
    Object value = env.get(symbol, null, null);
    if (value == null)
      throw new RuntimeException("unbound external "+name);
    if (type instanceof ClassType)
      {
        String cname = ((ClassType) type).getName();
        // KLUDGE - FIXME
        if ("gnu.math.IntNum".equals(cname))
          value = IntNum.valueOf(value.toString());
        else if ("gnu.math.RealNum".equals(cname))
          value = gnu.math.DFloNum.make(Double.parseDouble(value.toString()));
        else
          throw new Error("cast to "+cname+" for external "
                          +name+" not implemented");
      }
    return value;
  }
}

class Prompter extends Procedure1
{
  public Object apply1 (Object arg)
  {
    InPort port = (InPort) arg;
    int line = port.getLineNumber() + 1;
    char state = port.readState;
    if (state == '\n')
      state = ' ';
    if (state == '<')
      return "<!--" + line + "-->";
    else if (state == ':')
      return "-(:" + line + "c:) ";
    else
      return "(: " + line + state + ":) ";
  }
}
