package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/**
  * This class represents try/catch/finally.
  * @author      Per Bothner
  */

public class TryExp extends Expression
{
  Expression try_clause;

  CatchClause catch_clauses;

  Expression finally_clause;
  
  public final CatchClause getCatchClauses () { return catch_clauses; }
  public final void setCatchClauses (CatchClause catch_clauses)
  {
    this.catch_clauses = catch_clauses;
  }

  public TryExp (Expression try_clause, Expression finally_clause)
  {
    this.try_clause = try_clause;
    this.finally_clause = finally_clause;
  }

  protected boolean mustCompile () { return catch_clauses != null; }

  public void apply (CallContext ctx) throws Throwable
  {
    if (catch_clauses != null)
      throw new RuntimeException("internal error - TryExp.eval called");
    try
      {
	try_clause.apply(ctx);
        ctx.runUntilDone();
      }
    catch (Throwable ex)
      {
        for (CatchClause clause = catch_clauses; clause != null;
             clause = clause.next)
          {
            Declaration decl = clause.firstDecl();
            // FIXME
          }
        throw ex;
      }
    finally
      {
	finally_clause.eval(ctx);
      }
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    boolean has_finally = finally_clause != null;
    Type result_type = target instanceof IgnoreTarget ? null
	: getType();
    Target ttarg;
    if (result_type == null)
      ttarg = Target.Ignore;
    else if (result_type == Type.pointer_type)
      ttarg = Target.pushObject;
    else
      ttarg = new StackTarget(result_type);
    code.emitTryStart(has_finally, result_type);
    try_clause.compileWithPosition(comp, ttarg);
    code.emitTryEnd();

    CatchClause catch_clause = catch_clauses;
    for (; catch_clause != null;  catch_clause = catch_clause.getNext())
      {
	catch_clause.compile(comp, ttarg);
      }

    if (finally_clause != null)
      {
	code.emitFinallyStart();
	finally_clause.compileWithPosition(comp, Target.Ignore);
	code.emitFinallyEnd();
      }
    code.emitTryCatchEnd();
    if (result_type != null)
      target.compileFromStack(comp, result_type);
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkTryExp(this);
  }

  protected void walkChildren(ExpWalker walker)
  {
    try_clause = walker.walk(try_clause);
    CatchClause catch_clause = catch_clauses;
    while (walker.exitValue == null && catch_clause != null)
      {
	walker.walk(catch_clause);
	catch_clause = catch_clause.getNext();
      }

    if (walker.exitValue == null && finally_clause != null)
      finally_clause =walker.walk( finally_clause);
  }

  public void print (OutPort ps)
  {
    ps.startLogicalBlock("(Try", ")", 2);
    ps.writeSpaceFill();
    try_clause.print(ps);
    CatchClause catch_clause = catch_clauses;
    for (; catch_clause != null;  catch_clause = catch_clause.getNext())
      {
	catch_clause.print(ps);
      }
    if (finally_clause != null)
      {
	ps.writeSpaceLinear();
	ps.print(" finally: ");
	finally_clause.print(ps);
      }
    ps.endLogicalBlock(")");
  }
}
