package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.OutPort;

/** A "catch" clause of a "try-catch" form.
  */

public class CatchClause extends ScopeExp
{
  Expression body;
  CatchClause next;

  public CatchClause (String name, ClassType type)
  {
    super ();
    addDeclaration (name, type);
  }

  public final CatchClause getNext() { return next; }
  public final void setNext (CatchClause next) { this.next = next; }

  public final Expression getBody() { return body; }
  public final void setBody(Expression body) { this.body = body; }

  public void compile (Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    Declaration catchDecl = firstDecl();
    Variable catchVar = catchDecl.allocateVariable(code);
    code.enterScope (scope);
    code.emitCatchStart(catchVar);
    body.compileWithPosition(comp, target);
    code.emitCatchEnd();
    code.popScope ();
  }

  protected void walkChildren(ExpWalker walker)
  {
    body = body.walk(walker);
  }

  public void print (OutPort ps)
  {
    ps.print("(Catch ? ");
    body.print(ps);
    ps.print(")");
  }
}
