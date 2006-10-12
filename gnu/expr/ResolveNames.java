// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;

/** This resolves references to lexical Declarations.
 * So far it is only used for XQuery, which overrides it. */

public class ResolveNames extends ExpWalker
{
  protected NameLookup lookup;

  public ResolveNames ()
  {
  }

  public ResolveNames (Compilation comp)
  {
    setContext(comp);
    lookup = comp.lexical;
  }

  public void resolveModule(ModuleExp exp)
  {
    Compilation save_comp = Compilation.getCurrent();
    try
      {
        if (comp != null)
          Compilation.setCurrent(comp);
        push(exp);
        exp.walkChildren(this);
      }
    finally
      {
        Compilation.setCurrent(save_comp);
        // Note we don't do lookup.pop(exp).  This is so top-level
        // declarations remain for future uses of the same Lexer.
      }
  }

  protected void push (ScopeExp exp)
  {
    lookup.push(exp);
  }

  protected Expression walkScopeExp (ScopeExp exp)
  {
    push(exp);
    exp.walkChildren(this);
    lookup.pop(exp);
    return exp;
  }

  protected Expression walkLetExp (LetExp exp)
  {
    exp.walkInitializers(this);
    push(exp);
    exp.body = (Expression) walk(exp.body);
    lookup.pop(exp);
    return exp;
  }

  public Declaration lookup (Expression exp, Object symbol, boolean function)
  {
    return lookup.lookup(symbol, function);
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl == null)
      {
	decl = lookup(exp, exp.getSymbol(), exp.isProcedureName());
	if (decl != null)
	  exp.setBinding(decl);
      } 
    return exp;
 }

  protected Expression walkSetExp (SetExp exp)
  {
    if (exp.binding == null)
      exp.binding = lookup(exp, exp.getSymbol(), exp.isFuncDef());
    return super.walkSetExp(exp);
  }
}
