// Copyright (c) 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.xml.NamespaceBinding;
import gnu.mapping.*;
import gnu.bytecode.ClassType;
import gnu.kawa.reflect.*;

public class XQResolveNames extends ResolveNames
{
  /** Value of <code>getCode()</code> for <code>lastDecl</code>. */
  public static final int LAST_BUILTIN = -1;

  /** Value of <code>getCode()</code> for <code>positionDecl</code>. */
  public static final int POSITION_BUILTIN = -2;

  /** Declaration for the <code>fn:last()</code> function. */
  public static final Declaration lastDecl
    = makeBuiltin("last", LAST_BUILTIN);

  /** Declaration for the <code>fn:position()</code> function. */
  public static final Declaration positionDecl
    = makeBuiltin("position", POSITION_BUILTIN);

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

  public XQResolveNames (Compilation comp)
  {
    super(comp);
    lookup.push(lastDecl);
    lookup.push(positionDecl);
  }

  public Namespace[] functionNamespacePath
    = XQuery.defaultFunctionNamespacePath;

  protected Symbol namespaceResolve (String name, boolean function)
  {
    int colon = name.indexOf(':');
    String prefix = colon >= 0 ? name.substring(0, colon)
      : function ? XQuery.DEFAULT_FUNCTION_PREFIX
      : XQuery.DEFAULT_ELEMENT_PREFIX; 
    String nssym = (Interpreter.NAMESPACE_PREFIX + prefix).intern();
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
	    if (name != null)
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

  Declaration flookup (Symbol sym, Environment env)
  {
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

  public Declaration lookup (Expression exp, Object symbol, boolean function)
  {
    Declaration decl = lookup.lookup(symbol, function);
    if (decl != null)
      return decl;
    if (symbol instanceof String)
      {
	String name = (String) symbol;
	if (function && name.indexOf(':') < 0)
	  {
	    Environment builtins = XQuery.getInstance().getEnvironment();
	    for (int i = 0;  i < functionNamespacePath.length;  i++)
	      {
		Symbol sym = functionNamespacePath[i].lookup(name);
		if (sym != null)
		  {
		    decl = lookup.lookup(sym, function);
		    if (decl != null)
		      return decl;
		    if (! function)
		      continue;
		    decl = flookup(sym, builtins);
		    if (decl != null)
		      return decl;
		  }
	      }
	  }
	else
	  {
	    Symbol sym = namespaceResolve(name, function);
	    if (sym == null)
	      return null;
	    decl = lookup.lookup(sym, function);
	    if (function && decl == null)
	      {
		String uri = sym.getNamespaceURI();
		if (uri != null && uri.length() > 6 &&
		    uri.startsWith("class:"))
		  {
		    ClassType ctype = ClassType.make(uri.substring(6));
		    return procToDecl(sym,
				      ClassMethodProc.make(ctype, sym.getName()));
		  }
		Environment builtins = XQuery.getInstance().getEnvironment();
		decl = flookup(sym, builtins);
		if (decl != null)
		  return decl;
	      }
	  }
      }
    if (decl == null)
      {
	if (function)
	  {
	  }
	error('e',
	      (function ? "unknown function " : "unknown variable $")+symbol);
      }
    return decl;
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    if (exp.getBinding() == null)
      {
	Object symbol = exp.getSymbol();
	Declaration decl = lookup.lookup(symbol, exp.isProcedureName());
	if (decl != null)
	  exp.setBinding(decl);
	else
	  {
	    Symbol sym;
	    if (symbol instanceof Symbol
		&& "".equals((sym = (Symbol) symbol).getNamespaceURI()))
	      {
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
	    super.walkReferenceExp(exp);
	  }
      }
    return exp;
  }

  protected Expression walkSetExp (SetExp exp)
  {
    Expression result = super.walkSetExp(exp);
    Declaration decl = exp.binding;
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

  protected Expression walkApplyExp (ApplyExp exp)
  {
    super.walkApplyExp(exp);
    Expression func = exp.getFunction();
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
	      }
	  }
      }
    Object proc = exp.getFunctionValue();
    if (proc instanceof AttributeConstructor)
      {
	AttributeConstructor cons = (AttributeConstructor) proc;
	if (cons.getQName() == null)
	  cons.setQName(namespaceResolve(cons.getXmlName(), false));
      }
    else if (proc instanceof ElementConstructor)
      {
	ElementConstructor cons = (ElementConstructor) proc;
	if (cons.getQName() == null)
	  {
	    Compilation comp = getCompilation();
	    int saveColumn = comp.getColumn();
	    // Add 1 for the '<' to get the actual element name.
	    if (saveColumn > 0)
	      comp.setColumn(saveColumn+1);
	    cons.setQName(namespaceResolve(cons.getXmlName(), false));
	    comp.setColumn(saveColumn);
	  }

	// Add namespaces nodes that might be needed.
	NamespaceBinding nsBindings = cons.getNamespaceNodes();
	nsBindings = maybeAddNamespace(cons.getXmlName(),
				       cons.getQName(), nsBindings);
	Expression[] args = exp.getArgs();
	for (int i = 0;  i < args.length;  i++)
	  {
	    Expression arg = args[i++];
	    if (arg instanceof ApplyExp
		&& (proc = ((ApplyExp) arg).getFunctionValue()) instanceof AttributeConstructor)
	      {
		AttributeConstructor acons = (AttributeConstructor) proc;
		nsBindings = maybeAddNamespace(acons.getXmlName(),
					       acons.getQName(), nsBindings);
	      }
	  }
	if (nsBindings != null)
	  cons.setNamespaceNodes(nsBindings);
      }
    return exp;
  }

  static NamespaceBinding maybeAddNamespace(String sname, Symbol qname,
					    NamespaceBinding bindings)
  {
    if (qname == null) // Happens if prevously-reported unknown prefix.
      return bindings;
    int colon = sname.indexOf(':');
    String prefix = colon < 0 ? null : sname.substring(0, colon).intern();
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
