// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;

/** This resolves references to lexical Declarations. */

public class ResolveNames extends ExpWalker
{
  NameLookup lookup;

  public static void resolveNames(Expression exp, NameLookup lookup)
  {
    ResolveNames walker = new ResolveNames();
    walker.lookup = lookup;
    walker.walk(exp);
  }

  protected Expression walkScopeExp (ScopeExp exp)
  {
    lookup.push(exp);
    exp.walkChildren(this);
    lookup.pop(exp);
    return exp;
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl == null)
      {
	decl = lookup.lookup(exp.getSymbol(), exp.isProcedureName());
	if (decl != null)
	  exp.setBinding(decl);
      } 
    return exp;
 }

  protected Expression walkSetExp (SetExp exp)
  {
    Declaration decl = exp.binding;
    if (decl == null)
      {
	decl = lookup.lookup(exp.getSymbol(), exp.isFuncDef());
	if (decl != null)
	  exp.binding = decl;
      }
    return super.walkSetExp(exp);
  }
}
