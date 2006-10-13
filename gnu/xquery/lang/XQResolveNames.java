// Copyright (c) 2003, 2004, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.xml.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.StaticFieldLocation;
import gnu.kawa.functions.GetNamedPart;
import gnu.xquery.util.NamedCollator;
import gnu.xquery.util.QNameUtils;
import java.util.Vector;
import gnu.math.DateTime;
import gnu.math.IntNum;

public class XQResolveNames extends ResolveNames
{
  XQParser parser;

  /** Code number for the special <code>last</code> function. */
  public static final int LAST_BUILTIN = -1;

  /** Code number for the special <code>position</code> function. */
  public static final int POSITION_BUILTIN = -2;

  /** Value of {@code xs:QName()} constructor. */
  public static final int XS_QNAME_BUILTIN = -3;

  /** Code number for the special <code>compare</code> function. */
  public static final int COMPARE_BUILTIN = -4;

  /** Code number for the special <code>distinct-values</code> function. */
  public static final int DISTINCT_VALUES_BUILTIN = -5;

  /** Code number for the special <code>local-name</code> function. */
  public static final int LOCAL_NAME_BUILTIN = -6;

  /** Code number for the special <code>namespace-uri</code> function. */
  public static final int NAMESPACE_URI_BUILTIN = -7;

  /** Code number for the special <code>root</code> function. */
  public static final int ROOT_BUILTIN = -8;

  /** Code number for the special <code>doc</code> function. */
  public static final int DOC_BUILTIN = -9;

  /** Code number for the special <code>doc-available</code> function. */
  public static final int DOC_AVAILABLE_BUILTIN = -10;

  /** Code number for the special <code>doc-available</code> function. */
  public static final int BASE_URI_BUILTIN = -11;

  /** Code number for the special <code>ressolve-uri</code> function. */
  public static final int RESOLVE_URI_BUILTIN = -12;

  /** Code number for internal function that maps prefix to uri. */
  public static final int RESOLVE_PREFIX_BUILTIN = -13;

  /** Code number for the special <code>static-base-uri</code> function. */
  public static final int STATIC_BASE_URI_BUILTIN = -14;

  /** Code number for the special <code>index-of</code> function. */
  public static final int INDEX_OF_BUILTIN = -15;

  /** Code number for the special <code>string</code> function. */
  public static final int STRING_BUILTIN = -16;

  /** Code number for the special <code>normalize-space</code> function. */
  public static final int NORMALIZE_SPACE_BUILTIN = -17;

  /** Code number for the special <code>unordered</code> function. */
  public static final int UNORDERED_BUILTIN = -18;

  /** Code number for the special <code>current-dateTime</code> function. */
  public static final int CURRENT_DATETIME_BUILTIN = -19;

  /** Code number for the special <code>current-date</code> function. */
  public static final int CURRENT_DATE_BUILTIN = -20;

  /** Code number for the special <code>current-time</code> function. */
  public static final int CURRENT_TIME_BUILTIN = -21;

  /** Code number for the special <code>implicit-timezone</code> function. */
  public static final int IMPLICIT_TIMEZONE_BUILTIN = -22;

  /** Code number for the special <code>lang</code> function. */
  public static final int LANG_BUILTIN = -23;

  /** Code number for the special <code>name</code> function. */
  public static final int NAME_BUILTIN = -24;

  /** Code number for the special <code>deep-equal</code> function. */
  public static final int DEEP_EQUAL_BUILTIN = -25;

  /** Code number for the special <code>min</code> function. */
  public static final int MIN_BUILTIN = -26;

  /** Code number for the special <code>max</code> function. */
  public static final int MAX_BUILTIN = -27;

  /** Code number for the special <code>number</code> function. */
  public static final int NUMBER_BUILTIN = -28;

  /** Declaration for the <code>fn:last()</code> function. */
  public static final Declaration lastDecl
    = makeBuiltin("last", LAST_BUILTIN);

  public Declaration currentDateTimeDecl;
  public Declaration currentDateDecl;
  public Declaration currentTimeDecl;
  public Declaration currentTimezoneDecl;

  public static final Declaration xsQNameDecl
    = makeBuiltin(Symbol.make(XQuery.SCHEMA_NAMESPACE, "QName"), XS_QNAME_BUILTIN);

  public static final Declaration resolvePrefixDecl
    = makeBuiltin(Symbol.make(XQuery.SCHEMA_NAMESPACE, "(resolve-prefix)"),
                  RESOLVE_PREFIX_BUILTIN);


  /** Create a <code>Declaration</code> for a builtin function. */
  public static Declaration makeBuiltin (String name, int code)
  {
    return makeBuiltin (Symbol.make(XQuery.XQUERY_FUNCTION_NAMESPACE, name),
			code);
  }

  /** Create a <code>Declaration</code> for a builtin function. */
  public static Declaration makeBuiltin (Symbol name, int code)
  {
    Declaration decl = new Declaration(name);
    decl.setProcedureDecl(true);
    decl.setCode(code);
    return decl;
  }

  public XQResolveNames ()
  {
    this(null);
  }

  void pushBuiltin (String name, int code)
  {
    lookup.push(makeBuiltin(name, code));
  }

  public XQResolveNames (Compilation comp)
  {
    super(comp);
    lookup.push(lastDecl);
    lookup.push(xsQNameDecl);
    pushBuiltin("position", POSITION_BUILTIN);
    pushBuiltin("compare", COMPARE_BUILTIN);
    pushBuiltin("distinct-values", DISTINCT_VALUES_BUILTIN);
    pushBuiltin("local-name", LOCAL_NAME_BUILTIN);
    pushBuiltin("name", NAME_BUILTIN);
    pushBuiltin("namespace-uri", NAMESPACE_URI_BUILTIN);
    pushBuiltin("root", ROOT_BUILTIN);
    pushBuiltin("base-uri", BASE_URI_BUILTIN);
    pushBuiltin("lang", LANG_BUILTIN);
    pushBuiltin("static-base-uri", STATIC_BASE_URI_BUILTIN);
    pushBuiltin("resolve-uri", RESOLVE_URI_BUILTIN);
    pushBuiltin("doc", DOC_BUILTIN);
    pushBuiltin("document", DOC_BUILTIN); // Obsolete
    pushBuiltin("doc-available", DOC_AVAILABLE_BUILTIN);
    pushBuiltin("index-of", INDEX_OF_BUILTIN);
    pushBuiltin("string", STRING_BUILTIN);
    pushBuiltin("normalize-space", NORMALIZE_SPACE_BUILTIN);
    pushBuiltin("unordered", UNORDERED_BUILTIN);
    pushBuiltin("current-dateTime", CURRENT_DATETIME_BUILTIN);
    pushBuiltin("current-date", CURRENT_DATE_BUILTIN);
    pushBuiltin("current-time", CURRENT_TIME_BUILTIN);
    pushBuiltin("implicit-timezone", IMPLICIT_TIMEZONE_BUILTIN);
    pushBuiltin("deep-equal", DEEP_EQUAL_BUILTIN);
    pushBuiltin("min", MIN_BUILTIN);
    pushBuiltin("max", MAX_BUILTIN);
    pushBuiltin("number", NUMBER_BUILTIN);
  }

  public Namespace[] functionNamespacePath
    = XQuery.defaultFunctionNamespacePath;

  protected Symbol namespaceResolve (String name, boolean function)
  {
    int colon = name.indexOf(':');
    String prefix = colon >= 0 ? name.substring(0, colon)
      : function ? XQuery.DEFAULT_FUNCTION_PREFIX
      : XQuery.DEFAULT_ELEMENT_PREFIX; 
    String nssym = prefix.intern();
    Declaration decl = lookup.lookup(nssym, -1);
    Object uri = null;
    if (decl != null)
      uri = decl.getConstantValue();

    if (! (uri instanceof String))
      {
	if (colon < 0)
	  uri = "";
	else
	  {
	    try
	      {
		Class cl = Class.forName(prefix);
		uri = "class:" + prefix;
	      }
	    catch (Exception ex)
	      {
		messages.error('e',
                               "unknown namespace prefix '" + prefix + "'",
                               "XPST0081");
		return null;
	      }
	  }
      }
    String local = colon < 0 ? name : name.substring(colon+1);
    return Symbol.make(uri, local);
  }

  protected void push (ScopeExp exp)
  {
    Compilation comp = getCompilation();
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.isNamespaceDecl())
	  lookup.push(decl);
      }
    // Can't do namespace resolution until now.
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.isNamespaceDecl())
	  continue;
	Object name = decl.getSymbol();
	boolean function = decl.isProcedureDecl();
	if (name instanceof String)
	  {
	    int line = decl.getLine();
	    if (line > 0 && comp != null)
	      {
		String saveFilename = comp.getFile();
		int saveLine = comp.getLine();
		int saveColumn = comp.getColumn();
		comp.setLine(decl.getFile(), line, decl.getColumn());
		name = namespaceResolve((String) name, function);
		comp.setLine(saveFilename, saveLine, saveColumn);
	      }
	    else
	      name = namespaceResolve((String) name, function);
	    if (name == null)
              continue;
            decl.setName(name);
	  }

	Declaration old = lookup.lookup(name, function);
	if (XQParser.warnHidePreviousDeclaration
            && old != null
	    && (! (name instanceof Symbol)
		|| ((Symbol) name).getNamespace() != null))
	  comp.error('w', decl, "declaration ",
		     " hides previous declaration");
	lookup.push(decl);
      }
  }

  Declaration flookup (Symbol sym)
  {
    Environment env = XQuery.xqEnvironment;
    gnu.mapping.Location loc = env.lookup(sym, EnvironmentKey.FUNCTION);
    if (loc == null)
      return null;
    loc = loc.getBase();
    if (loc instanceof StaticFieldLocation)
      {
	Declaration decl = ((StaticFieldLocation) loc).getDeclaration();
	if (decl != null)
	  return decl;
      }
    Object val = loc.get(null);
    if (val != null)
      return procToDecl(sym, val);
    return null;
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    if (exp.getBinding() == null)
      {
	Object symbol = exp.getSymbol();
        boolean function = exp.isProcedureName();
	Declaration decl = lookup.lookup(symbol, function);
        Symbol sym;
        if (decl != null)
          ;
	else if (symbol instanceof Symbol
            && "".equals((sym = (Symbol) symbol).getNamespaceURI()))
          {
            // kludge - use xxx_BUILTIN mechanism?  FIXME
            String name = sym.getLocalName();
            Expression f;
            if ("request".equals(name))
              f = XQParser.makeFunctionExp("gnu.kawa.servlet.GetRequest", 
                                           "getRequest");
            else if ("response".equals(name))
              f = XQParser.makeFunctionExp("gnu.kawa.servlet.GetResponse",
                                           "getResponse");
            else
              f = null;
            if (f != null)
              return new ApplyExp(f, Expression.noExpressions);
          }
        else if (symbol instanceof Symbol)
          {
            // Never happens, I believe.
            decl = flookup((Symbol) symbol);
          }
        else // if (symbol instanceof String)
          {
            String name = (String) symbol;
            if (function && name.indexOf(':') < 0)
              {
                for (int i = 0;  i < functionNamespacePath.length;  i++)
                  {
                    sym = functionNamespacePath[i].lookup(name);
                    if (sym != null)
                      {
                        decl = lookup.lookup(sym, function);
                        if (decl != null)
                          break;
                        if (! function)
                          continue;
                        decl = flookup(sym);
                        if (decl != null)
                          break;
                      }
                  }
              }
            else
              {
                sym = namespaceResolve(name, function);
                if (sym != null)
                  {
                    decl = lookup.lookup(sym, function);
                    if (decl == null && function)
                      {
                        String uri = sym.getNamespaceURI();
                        if (XQuery.SCHEMA_NAMESPACE.equals(uri))
                          {
                            Type type =
                              XQuery.getStandardType(sym.getName());
                            if (type != null)
                              return QuoteExp.getInstance(type);
                          }
                        if (uri != null && uri.length() > 6 &&
                            uri.startsWith("class:"))
                          {
                            ClassType ctype = ClassType.make(uri.substring(6));
                            return GetNamedPart.makeExp(ctype, sym.getName());
                          }
                        decl = flookup(sym);
                      }
                  }
              }
          }
        if (decl != null)
          exp.setBinding(decl);
        else if (function)
          error('e', "unknown function "+symbol);
        else
          messages.error('e',"unknown variable $"+symbol, "XPST0008");
      }
    return exp;
  }

  protected Expression walkSetExp (SetExp exp)
  {
    Expression result = super.walkSetExp(exp);
    Declaration decl = exp.getBinding();
    Object name;
    if (decl != null && ! getCompilation().immediate
	&& (name = decl.getSymbol()) instanceof Symbol
	&& XQuery.LOCAL_NAMESPACE.equals(((Symbol) name).getNamespaceURI()))
      {
	decl.setFlag(Declaration.PRIVATE_SPECIFIED);
	decl.setPrivate(true);
      }
    return result;
  }

  public void resolveModule(ModuleExp exp)
  {
    super.resolveModule(exp);

    if (currentDateTimeDecl != null)
      {
        // Some expression in this module calls one of the current-XXX
        // functions, so calculate it and stash it, since these functions
        // are required to be 'stable'.
        // PROBLEM: We may get different results for calls in different
        // modules.  These functions should stash the current-dataTime in
        // in the actual dynamic context.  FIXME.
        Vector vec = new Vector();
        vec.addElement(new SetExp(currentDateTimeDecl,
                                  new ApplyExp(ClassType.make("gnu.xquery.util.TimeUtils").getDeclaredMethod("now", 0),
                                               Expression.noExpressions)));
        Method cast = ClassType.make("gnu.math.DateTime").getDeclaredMethod("cast", 1);
        if (currentDateDecl != null)
          {
            Expression[] args = { new ReferenceExp(currentDateTimeDecl),
                                  new QuoteExp(IntNum.make(DateTime.DATE_MASK)) };
            vec.addElement(new SetExp(currentDateDecl,
                                      new ApplyExp(cast, args)));
          }
        if (currentTimeDecl != null)
          {
            Expression[] args = { new ReferenceExp(currentDateTimeDecl),
                                  new QuoteExp(IntNum.make(DateTime.TIME_MASK)) };
            vec.addElement(new SetExp(currentTimeDecl,
                                      new ApplyExp(cast, args)));
          }
        if (currentTimezoneDecl != null)
          {
            Expression[] args = { new ReferenceExp(currentDateTimeDecl) };
            vec.addElement(new SetExp(currentTimezoneDecl,
                                      new ApplyExp(ClassType.make("gnu.xquery.util.TimeUtils").getDeclaredMethod("timezoneFromDateTime", 1), args)));
          }
        Expression body = exp.body;
        Expression[] exps;
        if (body instanceof BeginExp)
          {
            BeginExp bexp = (BeginExp) body;
            int blen = bexp.getExpressionCount();
            exps = bexp.getExpressions();
            for (int i = 0;  i < blen;  i++)
              vec.addElement(exps[i]);
          }
        else
          vec.addElement(body);
        exps = new Expression[vec.size()];
        vec.copyInto(exps);
        exp.body = new BeginExp(exps);
      }
  }

  NamespaceBinding constructorNamespaces;

  /**
   * Coerce argument to NamedCallator, or return default collator.
   * @param args argument list
   * @param argno index in args of collator argument
   */
  Expression getCollator (Expression[] args, int argno)
  {
    if (args != null && args.length > argno)
      return new ApplyExp(ClassType.make("gnu.xquery.util.NamedCollator")
                            .getDeclaredMethod("find", 1),
                            new Expression[] { args[argno] });
    NamedCollator coll = parser.defaultCollator;
    return coll == null ? QuoteExp.nullExp : new QuoteExp(coll);
  }

  Expression withCollator (Method method, Expression[] args,
                           String name, int minArgs)
  {
    return withCollator(new QuoteExp(new PrimProcedure(method)),
                        args, name, minArgs);
  }

  /** Adjust call to add default collator if collator argument is missing. */
  Expression withCollator (Expression function, Expression[] args,
                           String name, int minArgs)
  {
    String err = WrongArguments.checkArgCount(name, minArgs, minArgs+1, args.length);
    if (err != null)
      return getCompilation().syntaxError(err);
    Expression[] xargs = new Expression[minArgs+1];
    System.arraycopy(args, 0, xargs, 0, minArgs);
    xargs[minArgs] = getCollator(args, minArgs);
    return new ApplyExp(function, xargs);
  }

  /** Adjust call to add default contex itemt if that argument is missing. */
  Expression withContext (Method method, Expression[] args,
                          String name, int minArgs)
  {
    String err = WrongArguments.checkArgCount(name, minArgs, minArgs+1,
                                              args.length);
    if (err != null)
      return getCompilation().syntaxError(err);
    if (args.length == minArgs)
      {
        Expression[] xargs = new Expression[minArgs+1];
        System.arraycopy(args, 0, xargs, 0, minArgs);
        Declaration dot = lookup.lookup(XQParser.DOT_VARNAME, -1);
        if (dot == null)
          {
            String message = "undefined context for " + name;
            messages.error('e', message, "XPDY0002");
            return new ErrorExp(message);
          }
        xargs[minArgs] = new ReferenceExp(dot);
        args = xargs;
      }
    return new ApplyExp(method, args);
  }

  private Expression checkArgCount (Expression[] args, Declaration decl,
                                    int min, int max)
  {
    String err = WrongArguments.checkArgCount("fn:"+decl.getName(),
                                              min, max, args.length);
    if (err == null)
      return null;
    else
      return getCompilation().syntaxError(err);
  }

  protected Expression walkApplyExp (ApplyExp exp)
  {
    Expression func = exp.getFunction();
    NamespaceBinding namespaceSave = constructorNamespaces;
    Object proc = exp.getFunctionValue();
    if (proc instanceof MakeElement)
      constructorNamespaces = ((MakeElement) proc).getNamespaceNodes();
    super.walkApplyExp(exp);
    constructorNamespaces = namespaceSave;
    func = exp.getFunction();
    if (func instanceof ReferenceExp)
      {
	Declaration decl = ((ReferenceExp) func).getBinding();
	int code;
        Expression err;
        ModuleExp mexp;
	if (decl != null && (code = decl.getCode()) < 0)
	  {
	    switch (code)
	      {
	      case POSITION_BUILTIN:
	      case LAST_BUILTIN:
		Symbol sym = code == LAST_BUILTIN ? XQParser.LAST_VARNAME
		  : XQParser.POSITION_VARNAME;
		decl = lookup.lookup(sym, -1);
		if (decl == null)
		  error('e', "undefined context for " + sym.getName());
		return new ReferenceExp(sym, decl);
	      case XS_QNAME_BUILTIN:
		{
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
		  if (args[0] instanceof QuoteExp)
		    {
		      try
			{
			  Object val = ((QuoteExp) args[0]).getValue();
			  val = QNameUtils.resolveQName(val,
                                                        constructorNamespaces,
                                                        parser.prologNamespaces);
			  return new QuoteExp(val);
			}
		      catch (RuntimeException ex)
			{
			  return getCompilation().syntaxError(ex.getMessage());
			}
		    }
		  Expression[] xargs = {
		    args[0],
		    new QuoteExp(constructorNamespaces),
		    new QuoteExp(parser.prologNamespaces) };
		  Method meth
		    = (ClassType.make("gnu.xquery.util.QNameUtils")
		       .getDeclaredMethod("resolveQName", 3));
		  ApplyExp app = new ApplyExp(meth, xargs);
		  app.setFlag(ApplyExp.INLINE_IF_CONSTANT);
		  return app;
		}
	      case RESOLVE_PREFIX_BUILTIN:
		{
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
		  if (args[0] instanceof QuoteExp)
		    {
		      try
			{
			  Object val = ((QuoteExp) args[0]).getValue();
			  val = QNameUtils.resolvePrefix(val.toString(),
                                                         constructorNamespaces,
                                                         parser.prologNamespaces);
			  return new QuoteExp(val);
			}
		      catch (RuntimeException ex)
			{
			  return getCompilation().syntaxError(ex.getMessage());
			}
		    }
		  Expression[] xargs = {
		    args[0],
		    new QuoteExp(constructorNamespaces),
		    new QuoteExp(parser.prologNamespaces) };
		  PrimProcedure pproc
		    = new PrimProcedure(ClassType.make("gnu.xquery.util.QNameUtils")
                                        .getDeclaredMethod("resolvePrefix", 3));
		  ApplyExp app = new ApplyExp(pproc, xargs);
		  app.setFlag(ApplyExp.INLINE_IF_CONSTANT);
		  return app;
		}
              case LOCAL_NAME_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("localName", 1);
                  return withContext(meth, exp.getArgs(), "fn:local-name", 0);
                }
              case NAME_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("name", 1);
                  return withContext(meth, exp.getArgs(), "fn:name", 0);
                }
              case NUMBER_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NumberValue")
                    .getDeclaredMethod("numberValue", 1);
                  return withContext(meth, exp.getArgs(), "fn:number", 0);
                }
              case ROOT_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.kawa.xml.Nodes")
                    .getDeclaredMethod("root", 1);
                  return withContext(meth, exp.getArgs(), "fn:root", 0);
                }
              case BASE_URI_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.kawa.functions.BaseUri")
                    .getDeclaredMethod("baseUri", 1);
                  return withContext(meth, exp.getArgs(), "fn:base-uri", 0);
                }
              case LANG_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("lang", 2);
                  return withContext(meth, exp.getArgs(), "fn:lang", 1);
                }

              case STATIC_BASE_URI_BUILTIN:
                {
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 0, 0)) != null)
                    return err;
                  String result = parser.baseURI;
                  return result == null ? QuoteExp.voidExp
                    : QuoteExp.getInstance(result);
                }
              case NAMESPACE_URI_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("namespaceURI", 1);
                  return withContext(meth, exp.getArgs(),
                                     "fn:namespace-uri", 0);
                }

              case NORMALIZE_SPACE_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.StringUtils")
                    .getDeclaredMethod("normalizeSpace", 1);
                  return withContext(meth, exp.getArgs(),
                                     "fn:normalize-space", 0);
                }

              case UNORDERED_BUILTIN:
                {
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
                  return args[0];
                }

              case COMPARE_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.StringUtils")
                    .getDeclaredMethod("compare", 3);
                  return withCollator(meth, exp.getArgs(), "fn:compare", 2);
                }

              case STRING_BUILTIN:
                return withContext(XQParser.stringValueMethod,
                                   exp.getArgs(), "fn:string", 0);

              case INDEX_OF_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.SequenceUtils")
                    .getDeclaredMethod("indexOf$X", 4);
                  return withCollator(meth, exp.getArgs(), "fn:index-of", 2);
                }
              case DOC_BUILTIN:
              case DOC_AVAILABLE_BUILTIN:
                {
                  Expression[] args = exp.getArgs();
                  ClassType cl = ClassType.make("gnu.kawa.xml.Document");
                  String mname;
                  if (code == DOC_BUILTIN)
                    {
                      mname = "parseCached";
                      if (parser.warnOldVersion
                          && "document".equals(decl.getName()))
                        getCompilation()
                          .error('w', "replace 'document' by 'doc'");
                    }
                  else
                    mname = "availableCached";
                  Method meth = cl.getDeclaredMethod(mname, 2);
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
                  Expression base = getBaseUriExpr();
                  return new ApplyExp(meth, new Expression[]{ args[0], base });
                }
              case RESOLVE_URI_BUILTIN:
                {
                  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 2)) != null)
                    return err;
                  Expression[] margs = new Expression[2];
                  margs[0] = args[0];
                  if (args.length == 1)
                    margs[1] = getBaseUriExpr();
                  else
                    margs[1] = args[1];
                  Method meth = ClassType.make("gnu.text.URI_utils")
                    .getDeclaredMethod("resolve", 2);
                  return new ApplyExp(meth, margs);
                }
              case DISTINCT_VALUES_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.DistinctValues")
                    .getDeclaredMethod("distinctValues$X", 3);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:distinct-values", 1);

                }
              case DEEP_EQUAL_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.SequenceUtils")
                    .getDeclaredMethod("deepEqual", 3);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:deep-equal", 2);
                }
              case MIN_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.MinMax")
                    .getDeclaredMethod("min", 2);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:min", 1);
                }
              case MAX_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.MinMax")
                    .getDeclaredMethod("max", 2);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:max", 1);
                }
              case CURRENT_DATETIME_BUILTIN:
                if ((err = checkArgCount(exp.getArgs(), decl, 0, 0)) != null)
                  return err;
                mexp = getCompilation().mainLambda;
                if (currentDateTimeDecl == null)
                  currentDateTimeDecl = mexp.addDeclaration("dateTime", XTimeType.dateTimeType);
                return new ReferenceExp(currentDateTimeDecl);
              case CURRENT_DATE_BUILTIN:
                if ((err = checkArgCount(exp.getArgs(), decl, 0, 0)) != null)
                  return err;
                mexp = getCompilation().mainLambda;
                if (currentDateTimeDecl == null)
                  currentDateTimeDecl = mexp.addDeclaration("dateTime", XTimeType.dateTimeType);
                if (currentDateDecl == null)
                  currentDateDecl = mexp.addDeclaration("date", XTimeType.dateType);
                return new ReferenceExp(currentDateDecl);
              case CURRENT_TIME_BUILTIN:
                if ((err = checkArgCount(exp.getArgs(), decl, 0, 0)) != null)
                  return err;
                mexp = getCompilation().mainLambda;
                if (currentDateTimeDecl == null)
                  currentDateTimeDecl = mexp.addDeclaration("dateTime", XTimeType.dateTimeType);
                if (currentTimeDecl == null)
                  currentTimeDecl = mexp.addDeclaration("time", XTimeType.timeType);
                return new ReferenceExp(currentTimeDecl);
              case IMPLICIT_TIMEZONE_BUILTIN:
                if ((err = checkArgCount(exp.getArgs(), decl, 0, 0)) != null)
                  return err;
                mexp = getCompilation().mainLambda;
                if (currentDateTimeDecl == null)
                  currentDateTimeDecl = mexp.addDeclaration("dateTime", XTimeType.dateTimeType);
                if (currentTimezoneDecl == null)
                  currentTimezoneDecl = mexp.addDeclaration("timezone", XTimeType.dayTimeDurationType);
                return new ReferenceExp(currentTimezoneDecl);
	      }
	  }
      }
    proc = exp.getFunctionValue();
    if (proc instanceof Type)
      {
        Expression[] args = exp.getArgs();
        if (args.length != 1)
          {
            messages.error('e', "type constructor requires a single argument");
            return exp;
          }
        return new ApplyExp(XQParser.makeFunctionExp("gnu.xquery.util.CastAs", "castAs"),
                            new Expression[] { exp.getFunction(), args[0] });
      }
    if (proc instanceof MakeElement)
      {
	MakeElement make = (MakeElement) proc;

	// Add namespaces nodes that might be needed.
	NamespaceBinding nsBindings = make.getNamespaceNodes();
	nsBindings = maybeAddNamespace(MakeElement.getTagName(exp),
				       nsBindings);
	Expression[] args = exp.getArgs();
        Symbol[] attrSyms = new Symbol[args.length];
        int nattrSyms = 0;
	for (int i = 0;  i < args.length;  i++)
	  {
	    Expression arg = args[i];
	    if (arg instanceof ApplyExp)
	      {
		ApplyExp app = (ApplyExp) arg;
		if (app.getFunction() == MakeAttribute.makeAttributeExp)
                  {
                    Symbol sym = MakeElement.getTagName(app);
                    if (sym != null)
                      {
                        for (int j = 0;  ;  j++)
                          {
                            if (j == nattrSyms)
                              {
                                attrSyms[nattrSyms++] = sym;
                                break;
                              }
                            if (sym.equals(attrSyms[j]))
                              {
                                getCompilation().setLine(app);
                                Symbol groupSym = MakeElement.getTagName(exp);
                                String groupName = groupSym == null ? null
                                  : groupSym.toString();
                                messages.error('e', NodeTree.duplicateAttributeMessage(sym, groupName), "XQST0040");
                              }
                          }
                      }
                    nsBindings = maybeAddNamespace(sym, nsBindings);
                  }
	      }
	  }
	if (nsBindings != null)
	  make.setNamespaceNodes(nsBindings);
      }
    return exp;
  }

  Expression getBaseUriExpr ()
  {
    Compilation comp = getCompilation();
    if (parser.baseURI != null)
      return QuoteExp.getInstance(parser.baseURI);
    else
      return gnu.kawa.functions.GetModuleClass.getModuleClassURI(comp);
  }

  static NamespaceBinding maybeAddNamespace(Symbol qname,
					    NamespaceBinding bindings)
  {
    if (qname == null) // Happens if prevously-reported unknown prefix.
      return bindings;
    String prefix = qname.getPrefix();
    String uri = qname.getNamespaceURI();
    return NamespaceBinding.maybeAdd(prefix == "" ? null : prefix,
                                     uri == "" ? null : uri,
                                     bindings);
  }

  /** Wrap a (known) procedure value as a Declaration. */
  static Declaration procToDecl (Object symbol, Object val)
  {
    Declaration decl = new Declaration(symbol);
    decl.setProcedureDecl(true);
    decl.noteValue(new QuoteExp(val));
    decl.setFlag(Declaration.IS_CONSTANT);
    return decl;
  }
}
