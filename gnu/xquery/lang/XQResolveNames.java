// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.xml.NamespaceBinding;
import gnu.mapping.Symbol;

public class XQResolveNames extends ResolveNames
{
  public XQResolveNames ()
  {
  }

  public XQResolveNames (Compilation comp)
  {
    super(comp);
  }

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
	if (colon >= 0)
	  {
	    error('e', "unknown namespace prefix '" + prefix + "'");
	    return null;
	  }
	uri = "";
      }
    String local = colon < 0 ? name : name.substring(colon+1);
    return Symbol.make(uri, local);
  }

  protected void push (ScopeExp exp)
  {
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
	if (name instanceof String)
	  {
	    int line = decl.getLine();
	    Compilation comp = getCompilation();
	    boolean function = decl.isProcedureDecl();
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
	lookup.push(decl);
      }
  }

  public Declaration lookup (Expression exp, Object symbol, boolean function)
  {
    Declaration decl = lookup.lookup(symbol, function);
    if (decl != null)
      return decl;
    if (symbol instanceof String)
      {
	Symbol sym = namespaceResolve ((String) symbol, function);
	if (sym == null)
	  return null;
	decl = lookup.lookup(sym, function);
      }
    if (decl == null
	&& (! (exp instanceof ReferenceExp)
	    // Allow unbound procedures - for now.
	    || ! ((ReferenceExp) exp).isProcedureName()))
      {
	error('e', "unknown variable $"+symbol);
      }
    return decl;
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    if (exp.getBinding() == null && exp.getSymbol() instanceof Symbol)
      {
	Symbol sym = (Symbol) exp.getSymbol();
	if ("".equals(sym.getNamespaceURI()))
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
      }
    return super.walkReferenceExp(exp);
  }

  protected Expression walkApplyExp (ApplyExp exp)
  {
    super.walkApplyExp(exp);
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
	  cons.setQName(namespaceResolve(cons.getXmlName(), false));

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
    int colon = sname.indexOf(':');
    String prefix = colon < 0 ? null : sname.substring(0, colon).intern();
    String uri = qname.getNamespaceURI();
    return NamespaceBinding.maybeAdd(prefix, uri == "" ? null : uri, bindings);
  }

}
