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

  public Object eval (Environment env)
  {
    if (catch_clauses != null)
      throw new RuntimeException("internal error - TryExp.eval called");
    try
      {
	return try_clause.eval(env);
      }
    finally
      {
	finally_clause.eval(env);
      }
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    boolean has_finally = finally_clause != null;
    Type result_type = target instanceof IgnoreTarget ? null
	: getType();
    code.emitTryStart(has_finally, result_type);
    try_clause.compileWithPosition(comp, target);
    code.emitTryEnd();

    CatchClause catch_clause = catch_clauses;
    for (; catch_clause != null;  catch_clause = catch_clause.getNext())
      {
	catch_clause.compile(comp, target);
      }

    if (finally_clause != null)
      {
	code.emitFinallyStart();
	finally_clause.compileWithPosition(comp, Target.Ignore);
	code.emitFinallyEnd();
      }
    code.emitTryCatchEnd();
  }

  Object walk (ExpWalker walker) { return walker.walkTryExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(%try ");
    try_clause.print(ps);
    CatchClause catch_clause = catch_clauses;
    for (; catch_clause != null;  catch_clause = catch_clause.getNext())
      {
	catch_clause.print(ps);
      }
    ps.print(" finally: ");
    finally_clause.print(ps);
    ps.print(")");
  }
}
