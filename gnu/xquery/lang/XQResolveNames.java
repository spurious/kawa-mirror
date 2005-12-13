// Copyright (c) 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.xml.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.*;
import gnu.xquery.util.NamedCollator;

public class XQResolveNames extends ResolveNames
{
  XQParser parser;

  /** Code number for the special <code>last</code> function. */
  public static final int LAST_BUILTIN = -1;

  /** Code number for the special <code>position</code> function. */
  public static final int POSITION_BUILTIN = -2;

  /** Value of <code>xs:QName()</code> constructor. */
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

  /** Declaration for the <code>fn:last()</code> function. */
  public static final Declaration lastDecl
    = makeBuiltin("last", LAST_BUILTIN);

  public static final Declaration xsQNameDecl
    = makeBuiltin(Symbol.make(XQuery.SCHEMA_NAMESPACE, "QName"), XS_QNAME_BUILTIN);

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
    pushBuiltin("namespace-uri", NAMESPACE_URI_BUILTIN);
    pushBuiltin("root", ROOT_BUILTIN);
    pushBuiltin("base-uri", BASE_URI_BUILTIN);
    pushBuiltin("doc", DOC_BUILTIN);
    pushBuiltin("document", DOC_BUILTIN); // Obsolete
    pushBuiltin("doc-available", DOC_AVAILABLE_BUILTIN);
  }

  public Namespace[] functionNamespacePath
    = XQuery.defaultFunctionNamespacePath;

  protected Symbol namespaceResolve (String name, boolean function)
  {
    int colon = name.indexOf(':');
    String prefix = colon >= 0 ? name.substring(0, colon)
      : function ? XQuery.DEFAULT_FUNCTION_PREFIX
      : XQuery.DEFAULT_ELEMENT_PREFIX; 
    String nssym = (Language.NAMESPACE_PREFIX + prefix).intern();
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
		error('e', "unknown namespace prefix '" + prefix + "'");
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
	if (old != null
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
                        if (uri != null && uri.length() > 6 &&
                            uri.startsWith("class:"))
                          {
                            ClassType ctype = ClassType.make(uri.substring(6));
                            return ClassMethodProc.makeExp(new QuoteExp(ctype),
                                                           new QuoteExp(sym.getName()));
                          }
                        decl = flookup(sym);
                      }
                  }
              }
          }
        if (decl != null)
          exp.setBinding(decl);
        else
          error('e',
                (function ? "unknown function " : "unknown variable $")+symbol);
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
          return getCompilation().syntaxError("undefined context for " + name);
        xargs[minArgs] = new ReferenceExp(dot);
        args = xargs;
      }
    return new ApplyExp(method, args);
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
		  // FIXME check that args.length == 1.
		  if (args[0] instanceof QuoteExp)
		    {
		      try
			{
			  Object val = ((QuoteExp) args[0]).getValue();
			  val = gnu.xquery.util.QNameUtils.resolveQName(val,
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
              case LOCAL_NAME_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("localName", 1);
                  return withContext(meth, exp.getArgs(), "fn:local-name", 0);
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
              case NAMESPACE_URI_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("namespaceURI", 1);
                  return withContext(meth, exp.getArgs(),
                                     "fn:namespace-uri", 0);
                }
              case COMPARE_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.StringValue")
                    .getDeclaredMethod("compare", 3);
                  return withCollator(meth, exp.getArgs(), "fn:compare", 2);
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
                  String err
                    = WrongArguments.checkArgCount("fn:"+decl.getName(),
                                                   1, 1, args.length);
                  if (err != null)
                    return getCompilation().syntaxError(err);
                  Expression base = QuoteExp.getInstance(parser.baseURI);
                  return new ApplyExp(meth, new Expression[]{ args[0], base });
                }
              case DISTINCT_VALUES_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.DistinctValues")
                    .getDeclaredMethod("distinctValues$X", 3);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:distinct-values", 1);
                }
	      }
	  }
      }
    proc = exp.getFunctionValue();
    if (proc instanceof MakeElement)
      {
	MakeElement make = (MakeElement) proc;

	// Add namespaces nodes that might be needed.
	NamespaceBinding nsBindings = make.getNamespaceNodes();
	nsBindings = maybeAddNamespace(MakeElement.getTagName(exp),
				       nsBindings);
	Expression[] args = exp.getArgs();
	for (int i = 0;  i < args.length;  i++)
	  {
	    Expression arg = args[i++];
	    if (arg instanceof ApplyExp)
	      {
		ApplyExp app = (ApplyExp) arg;
		if (app.getFunction() == MakeAttribute.makeAttributeExp)
		  nsBindings = maybeAddNamespace(MakeElement.getTagName(app),
						 nsBindings);
	      }
	  }
	if (nsBindings != null)
	  make.setNamespaceNodes(nsBindings);
      }
    return exp;
  }

  static NamespaceBinding maybeAddNamespace(SName qname,
					    NamespaceBinding bindings)
  {
    if (qname == null) // Happens if prevously-reported unknown prefix.
      return bindings;
    String prefix = qname.getPrefix();
    String uri = qname.getNamespaceURI();
    return NamespaceBinding.maybeAdd(prefix, uri == "" ? null : uri, bindings);
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
