package kawa.lang;
import gnu.bytecode.*;

/** A "catch" clause of a "try-catch" form.
  */

public class CatchClause extends ScopeExp
{
  Expression body;
  CatchClause next;

  public CatchClause (String name, ClassType type, Expression body)
  {
    super ();
    addDeclaration (name, type);
    this.body = body;
  }

  public CatchClause getNext () { return next; }

  public void compile (Compilation comp, int flags)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.enterScope (scope);
    body.compile_with_linenumber (comp, flags);
    code.popScope ();
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(%catch ? ");
    body.print(ps);
    ps.print(")");
  }
}
