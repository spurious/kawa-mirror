package kawa.lang;
import gnu.bytecode.*;

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
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    if (catch_clauses != null)
      throw new GenericError ("internal error - TryExp.eval called");
    try
      {
	return try_clause.eval(env);
      }
    finally
      {
	finally_clause.eval(env);
      }
  }

  public void compile (Compilation comp, int flags)
  {
    CodeAttr code = comp.getCode();
    boolean has_finally = finally_clause != null;
    // Non-null if we need a temporary to save the result.
    Variable saved_result;
    if (has_finally && (flags & Expression.IGNORED) == 0)
      {
	code.pushScope();
	saved_result = code.addLocal(getType());
      }
    else
      saved_result = null;
    code.emitTryStart(has_finally);
    try_clause.compile_with_linenumber(comp, flags);
    if (saved_result != null)
      code.emitStore(saved_result);
    code.emitTryEnd();

    CatchClause catch_clause = catch_clauses;
    for (; catch_clause != null;  catch_clause = catch_clause.getNext())
      {
	catch_clause.compile(comp, flags);
      }

    if (finally_clause != null)
      {
	code.emitFinallyStart();
	finally_clause.compile_with_linenumber(comp, flags|Expression.IGNORED);
	code.emitFinallyEnd();
      }
    code.emitTryCatchEnd();
    if (saved_result != null)
      {
	code.emitLoad(saved_result);
	code.popScope();
      }
  }

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
