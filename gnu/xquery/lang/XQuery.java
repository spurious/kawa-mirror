// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.Char;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.xquery.util.*;
import gnu.xml.*;
import gnu.kawa.reflect.ClassMethods;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import java.io.Reader;
import java.util.Vector;
import gnu.kawa.functions.ConstantFunction0;
import gnu.math.IntNum;

/** The XQuery language. */

public class XQuery extends Interpreter
{
  public static final String XQUERY_FUNCTION_NAMESPACE
    = "http://www.w3.org/2003/05/xpath-functions";
  public static final String KAWA_FUNCTION_NAMESPACE
    = "http://kawa.gnu.org/";
  static boolean charIsInt = false;

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
    XQParser parser = new XQParser(inp, messages);
    parser.interpreter = this;
    return parser;
  }

  /** Special parser flag used by <code>evalToFocusProc</code>. */
  public static final int PARSE_WITH_FOCUS = 0x10000;

  public Compilation parse(Lexer lexer, int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    Compilation tr = new Compilation(this, lexer.getMessages());
    tr.immediate = (options & PARSE_IMMEDIATE) != 0;
    tr.mustCompileHere();
    ModuleExp mexp = new ModuleExp();
    mexp.setFile(lexer.getName());
    tr.push(mexp);
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
    ResolveNames.resolveNames(mexp, tr.lexical);
    return tr;
  }

  public int getNamespaceOf(Declaration decl)
  {
    return decl.isProcedureDecl() ? FUNCTION_NAMESPACE : VALUE_NAMESPACE;
  }

  public void define(String sym, Object p)
  {
    if (p instanceof Procedure)
      Environment.defineFunction(environ, sym, p);
    else
      environ.define (sym, p);
  }

  protected void define_method(String name, String cname, String mname)
  {
    Environment.defineFunction(environ, name,
			       ClassMethods.apply(cname, mname));
  }

  public String getName()
  {
    return "XQuery";
  }

  static XQuery instance;

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
    ctx.setArgs(item, IntNum.make(position),IntNum.make(size));
    ctx.proc = proc;
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
	ctx.setArgs(item, IntNum.make(position),IntNum.make(size));
	ctx.proc = proc;
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
	    ctx.setArgs(v.getPosNext(ipos), IntNum.make(i), size);
	    ctx.proc = proc;
	    ctx.runUntilDone();
	    if (i == count)
	      break;
            ipos = v.nextPos(ipos);
	  }
      }
    else
      {
	IntNum one = IntNum.one();
	ctx.setArgs(values, one, one);
	ctx.proc = proc;
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
    Compilation comp = parse(port, messages, PARSE_WITH_FOCUS);
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    try
      {
	ModuleExp.evalModule(environ, ctx, comp);
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
   * <code>XQuery:evalWithFocus($xquery, "expr", $sequence)</code>
   */
  public void evalWithFocus$X (String expr,
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
   * <code>XQuery:evalWithFocus($xquery, "expr", $item, $pos, $size)</code>
   */
  public void evalWithFocus$X (String expr,
			       Object item, int position, int size,
			       CallContext ctx)
    throws Throwable
  {
    Procedure proc = evalToFocusProc(expr);
    ctx.setArgs(item, IntNum.make(position), IntNum.make(size));
    ctx.proc = proc;
  }

  public XQuery()
  {
    Environment scmEnv = Scheme.builtin();

    environ = Environment.getInstance(XQUERY_FUNCTION_NAMESPACE);
    environ.setPrevious(extensionsEnvEnv);

    ModuleBody.setMainPrintValues(true);

    if (instance == null)
      instance = this;

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
	loadClass("gnu.kawa.slib.HTTP");
	loadClass("gnu.kawa.slib.XStrings");
      }
    catch (Throwable ex)
      {
	// Ignore.  We get a ClassNotFoundException if gnu.kawa.slib.HTTP
	// was not built.  We get a NoClassDefFoundError if gnu.kawa.slib.HTTP
	// can't find servlets in the classpath.
      }
    finally
      {
	Environment.setCurrent(saveEnv);
      }

    define("document", gnu.kawa.xml.Document.document);
    define("doc", gnu.kawa.xml.Document.document);  // kludge
    define("unescaped-data", gnu.kawa.xml.MakeUnescapedData.unescapedData);
    define("item-at", gnu.xquery.util.ItemAt.itemAt);
    define("count", gnu.kawa.functions.CountValues.countValues);
    define("min", gnu.xquery.util.MinMax.min);
    define("max", gnu.xquery.util.MinMax.max);
    define("sum", gnu.xquery.util.Reduce.sum);
    define("avg", gnu.xquery.util.Average.avg);
    define("index-of", gnu.xquery.util.IndexOf.indexOf);
    define("last-index-of", gnu.xquery.util.LastIndexOf.lastIndexOf);
    define("sublist", gnu.xquery.util.SubList.subList);
    define("empty", gnu.xquery.util.IsEmptySequence.isEmptySequence);
    define("false", new ConstantFunction0("false", Boolean.FALSE));
    define("true", new ConstantFunction0("true", Boolean.TRUE));
    define("number", gnu.xquery.util.NumberValue.numberValue);
    define("string-value", gnu.xquery.util.StringValue.stringValue);
    define("string", gnu.xquery.util.StringValue.string);
    define("concat", new kawa.standard.string_append());

    define_method("trace", "gnu.xquery.util.Debug", "trace");
    define("write-to", gnu.kawa.xml.WriteTo.writeTo);
    define_field("iterator-items",
		 "gnu.kawa.xml.IteratorItems", "iteratorItems");
    define_field("list-items", "gnu.kawa.xml.ListItems", "listItems");
    define_field("base-uri", "gnu.kawa.functions.BaseUri", "baseUri");
    define_method("node-name", "gnu.kawa.xml.NodeName", "nodeName");
    define_method("root", "gnu.kawa.xml.Nodes", "root");
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

    define("distinct-nodes", gnu.kawa.xml.SortNodes.sortNodes);
  }

  public static XQuery getInstance()
  {
    if (instance == null)
      instance = new XQuery();
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    XQuery interp = new XQuery();
    Interpreter.defaultInterpreter = interp;
    Environment.setCurrent(interp.getEnvironment());
  }

  public Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return XQParser.readObject(in);
  }

  public static final XMLFormat writeFormat = new XMLFormat();

  public FormatToConsumer getFormat(boolean readable)
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
      "integer", "gnu.math.IntNum",
      "positiveInteger", "gnu.math.IntNum",
      "nonPositiveInteger", "gnu.math.IntNum",
      "negativeInteger", "gnu.math.IntNum",
      "nonNegativeInteger", "gnu.math.IntNum",
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
