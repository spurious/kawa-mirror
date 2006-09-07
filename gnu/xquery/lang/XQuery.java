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
import gnu.kawa.xml.*;

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

  public Compilation getCompilation (Lexer lexer, SourceMessages messages)
  {
    return new Compilation(this, messages, ((XQParser) lexer).lexical);
  }

  /** Special parser flag used by <code>evalToFocusProc</code>. */
  public static final int PARSE_WITH_FOCUS = 0x10000;

  public boolean parse (Compilation tr, int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    ModuleExp mexp = tr.mainLambda;
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    tr.mustCompileHere();
    XQParser parser = (XQParser) tr.lexer;
    if ((options & PARSE_ONE_LINE) != 0)
      {
	Expression sexp = parser.parse(tr);
	if (sexp == null)
	  return false;
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
	lexp.body = parser.parse(tr);
	tr.pop(lexp);
	mexp.body = lexp;
      }
    else
      {
	Vector exps = new Vector(10);
        Expression sexp = mexp.body;
        if (sexp instanceof BeginExp)
          {
            BeginExp bexp = (BeginExp) sexp;
            int blen = bexp.getExpressionCount();
            Expression[] bexps = bexp.getExpressions();
            for (int i = 0;  i < blen;  i++)
              exps.addElement(bexps[i]);
          }
        else if (sexp != null && sexp != QuoteExp.voidExp)
          {
            exps.addElement(sexp);
          }
	for (;;)
	  {
	    sexp = parser.parse(tr);
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

    XQResolveNames resolver = new XQResolveNames(tr);
    resolver.functionNamespacePath = parser.functionNamespacePath;
    resolver.parser = parser;
    resolver.resolveModule(mexp); // FIXME should move to resolve(Compilation)
    return true;
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
	ModuleExp.evalModule(Environment.getCurrent(), ctx, comp, null, null);
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

  // This field need to be public so that the findLiteral method in
  // gnu.expr.LitTable can find it.
  public static final XQuery instance = new XQuery();
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

    define_method("trace", "gnu.xquery.util.Debug", "trace");
    defProcStFld("write-to", "gnu.kawa.xml.WriteTo", "writeTo");
    defProcStFld("write-to-if-changed", "gnu.kawa.xml.WriteTo",
                 "writeToIfChanged");
    defProcStFld("iterator-items",
		 "gnu.kawa.xml.IteratorItems", "iteratorItems");
    defProcStFld("list-items", "gnu.kawa.xml.ListItems", "listItems");
    define_method("node-name", "gnu.kawa.xml.NodeName", "nodeName");
    define_method("data", "gnu.xquery.util.NodeUtils", "data$X");
    define_method("lower-case", "gnu.xquery.util.StringUtils", "lowerCase");
    define_method("upper-case", "gnu.xquery.util.StringUtils", "upperCase");
    define_method("substring", "gnu.xquery.util.StringUtils", "substring");
    define_method("string-length",
		  "gnu.xquery.util.StringUtils", "stringLength");
    define_method("substring-before",
		  "gnu.xquery.util.StringUtils", "substringBefore");
    define_method("substring-after",
		  "gnu.xquery.util.StringUtils", "substringAfter");
    define_method("translate", "gnu.xquery.util.StringUtils", "translate");
    define_method("encode-for-uri", "gnu.xquery.util.StringUtils",
                  "encodeForUri");
    define_method("iri-to-uri", "gnu.xquery.util.StringUtils", "iriToUri");
    define_method("escape-html-uri", "gnu.xquery.util.StringUtils",
                  "escapeHtmlUri");
    define_method("string-pad", "gnu.xquery.util.StringUtils", "stringPad");
    define_method("contains", "gnu.xquery.util.StringUtils", "contains");
    define_method("starts-with", "gnu.xquery.util.StringUtils", "startsWith");
    define_method("ends-with","gnu.xquery.util.StringUtils", "endsWith");
    define_method("string-join", "gnu.xquery.util.StringUtils", "stringJoin");
    define_method("concat", "gnu.xquery.util.StringUtils", "concat$V");
    define_method("matches", "gnu.xquery.util.StringUtils", "matches");
    define_method("replace", "gnu.xquery.util.StringUtils", "replace");
    define_method("string-to-codepoints", "gnu.xquery.util.StringUtils",
		  "stringToCodepoints$X");
    define_method("codepoints-to-string", "gnu.xquery.util.StringUtils",
		  "codepointsToString");

    define_method("abs", "gnu.xquery.util.NumberValue", "abs");
    define_method("floor", "gnu.xquery.util.NumberValue", "floor");
    define_method("ceiling", "gnu.xquery.util.NumberValue", "ceiling");
    define_method("round", "gnu.xquery.util.NumberValue", "round");
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

    define_method("years-from-duration", "gnu.xquery.util.TimeUtils",
                  "yearsFromDuration");
    define_method("months-from-duration", "gnu.xquery.util.TimeUtils",
                  "monthsFromDuration");
    define_method("days-from-duration", "gnu.xquery.util.TimeUtils",
                  "daysFromDuration");
    define_method("hours-from-duration", "gnu.xquery.util.TimeUtils",
                  "hoursFromDuration");
    define_method("minutes-from-duration", "gnu.xquery.util.TimeUtils",
                  "minutesFromDuration");
    define_method("seconds-from-duration", "gnu.xquery.util.TimeUtils",
                  "secondsFromDuration");
    define_method("year-from-dateTime", "gnu.xquery.util.TimeUtils",
                  "yearFromDateTime");
    define_method("month-from-dateTime", "gnu.xquery.util.TimeUtils",
                  "monthFromDateTime");
    define_method("day-from-dateTime", "gnu.xquery.util.TimeUtils",
                  "dayFromDateTime");
    define_method("hours-from-dateTime", "gnu.xquery.util.TimeUtils",
                  "hoursFromDateTime");
    define_method("minutes-from-dateTime", "gnu.xquery.util.TimeUtils",
                  "minutesFromDateTime");
    define_method("seconds-from-dateTime", "gnu.xquery.util.TimeUtils",
                  "secondsFromDateTime");
    define_method("timezone-from-dateTime", "gnu.xquery.util.TimeUtils",
                  "timezoneFromDateTime");
    define_method("year-from-date", "gnu.xquery.util.TimeUtils",
                  "yearFromDate");
    define_method("month-from-date", "gnu.xquery.util.TimeUtils",
                  "monthFromDate");
    define_method("day-from-date", "gnu.xquery.util.TimeUtils",
                  "dayFromDate");
    define_method("timezone-from-date", "gnu.xquery.util.TimeUtils",
                  "timezoneFromDate");
    define_method("hours-from-time", "gnu.xquery.util.TimeUtils",
                  "hoursFromTime");
    define_method("minutes-from-time", "gnu.xquery.util.TimeUtils",
                  "minutesFromTime");
    define_method("seconds-from-time", "gnu.xquery.util.TimeUtils",
                  "secondsFromTime");
    define_method("timezone-from-time", "gnu.xquery.util.TimeUtils",
                  "timezoneFromTime");
    define_method("adjust-dateTime-to-timezone", "gnu.xquery.util.TimeUtils",
                  "adjustDateTimeToTimezone"); // overloaded
    define_method("adjust-date-to-timezone", "gnu.xquery.util.TimeUtils",
                  "adjustDateTimeToTimezone"); // overloaded
    define_method("adjust-time-to-timezone", "gnu.xquery.util.TimeUtils",
                  "adjustDateTimeToTimezone"); // overloaded

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
    { "string", XDataType.stringType,
      "untypedAtomic", XDataType.untypedAtomicType,
      "boolean", XDataType.booleanType,
      "integer", XIntegerType.integerType,
      "long", XIntegerType.longType,
      "int", XIntegerType.intType,
      "short", XIntegerType.shortType,
      "byte", XIntegerType.byteType,
      "unsignedLong", XIntegerType.unsignedLongType,
      "unsignedInt", XIntegerType.unsignedIntType,
      "unsignedShort", XIntegerType.unsignedShortType,
      "unsignedByte", XIntegerType.unsignedByteType,
      "positiveInteger", XIntegerType.positiveIntegerType,
      "nonPositiveInteger", XIntegerType.nonPositiveIntegerType,
      "negativeInteger", XIntegerType.negativeIntegerType,
      "nonNegativeInteger", XIntegerType.nonNegativeIntegerType,
      "date", XTimeType.dateType,
      "dateTime", XTimeType.dateTimeType,
      "time", XTimeType.timeType,
      "duration", XTimeType.durationType,
      "yearMonthDuration", XTimeType.yearMonthDurationType,
      "dayTimeDuration", XTimeType.dayTimeDurationType,
      "gYearMonth", XTimeType.gYearMonthType,
      "gYear", XTimeType.gYearType,
      "gMonthDay", XTimeType.gMonthDayType,
      "gDay", XTimeType.gDayType,
      "gMonth", XTimeType.gMonthType,
      "decimal", XDataType.decimalType,
      "float", XDataType.floatType,
      "double", XDataType.doubleType,
      "anyURI", XDataType.anyURIType,
      "hexBinary", XDataType.hexBinaryType,
      "base64Binary", XDataType.base64BinaryType,
      "QName", "gnu.mapping.Symbol",
      "anyAtomicType", Type.pointer_type
    };

  public static Type getStandardType (String name)
  {
    for (int i = typeMap.length;  (i -= 2) >= 0; )
      {
	if (typeMap[i].equals(name))
	  {
	    Object t = typeMap[i+1];
	    if (t instanceof String)
	      return Scheme.string2Type((String) t);
	    else
	      return (Type) t;
	  }
      }
    return null;
  }

  public Type getTypeFor(String name)
  {
    String core = name.startsWith("xs:") ? name.substring(3)
      : name.startsWith("xdt:") ? name.substring(4)
      : name;
    Type t = getStandardType(core);
    return t != null ? t : Scheme.string2Type(name);
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
    else if (! clas.isArray())
      {
        String name = clas.getName();
        if (name.equals("java.lang.String"))
          return XDataType.stringType;
        if (name.equals("gnu.kawa.xml.UntypedAtomic"))
          return XDataType.untypedAtomicType;
        if (name.equals("java.lang.Boolean"))
          return XDataType.booleanType;
        if (name.equals("java.lang.Float"))
          return XDataType.floatType;
        if (name.equals("java.lang.Double"))
          return XDataType.doubleType;
        if (name.equals("java.math.BigDecimal"))
          return XDataType.decimalType;
        if (name.equals("gnu.math.Duration"))
          return XDataType.durationType;
        /* #ifdef use:java.net.URI */
        if (name.equals("java.net.URI"))
          return  XDataType.anyURIType;
        /* #endif */
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

  public static String makeClassName (String source)
  {
    source = source.replace(java.io.File.separatorChar, '/');
    int sl = source.lastIndexOf('/');
    if (sl >= 0)
      source = source.substring(sl+1);
    int dot = source.lastIndexOf('.');
    if (dot >= 0)
      source = source.substring(0, dot);
    return Compilation.mangleNameIfNeeded(source);
  }

  public static Object getExternal (Symbol name, Object type)
  {
    Environment env = Environment.getCurrent();
    Object value = env.get(name, null, null);
    if (value == null)
      throw new RuntimeException("unbound external "+name);
    if (type instanceof XDataType)
      return ((XDataType) type).cast(value);
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
