package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.OutPort;

/** A "catch" clause of a "try-catch" form.
  */

public class CatchClause extends ScopeExp
{
  Expression body;
  CatchClause next;

  public CatchClause (Object name, ClassType type)
  {
    super ();
    addDeclaration (name, type);
  }

  /** "Convert" a <code>LetExp</code> to a <code>CatchClause</code>. */
  public CatchClause (LetExp let)
  {
    Declaration decl = let.firstDecl();
    let.remove(null, decl);
    add(decl);
    body = let.body;
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
    code.enterScope(getVarScope());
    code.emitCatchStart(catchVar);
    body.compileWithPosition(comp, target);
    code.emitCatchEnd();
    code.popScope ();
  }

  protected void walkChildren(ExpWalker walker)
  {
    body = walker.walk(body);
  }

  public void print (OutPort out)
  {
    out.writeSpaceLinear();
    out.startLogicalBlock("(Catch ", ")", 2);
    decls.printInfo(out);
    out.writeSpaceLinear();
    body.print(out);
    out.endLogicalBlock(")");
  }
}
