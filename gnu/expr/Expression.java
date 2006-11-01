package gnu.expr;
import gnu.bytecode.Type;
import gnu.mapping.*;

/**
 * Abstract class for syntactic forms that evaluate to a value.
 * Scheme S-expressions get re-written to these before evaluation.
 * @author	Per Bothner
 */

public abstract class Expression extends Procedure0 implements Printable
{
  public final Object eval (CallContext ctx) throws Throwable
  {
    int start = ctx.startFromContext();
    try
      {
	match0(ctx);
	return ctx.getFromContext(start);
      }
    catch (Throwable ex)
      {
	ctx.cleanupFromContext(start);
	throw ex;
      }
  }

  public final Object eval (Environment env) throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    Environment save = ctx.getEnvironmentRaw();
    if (env != save)
      ctx.setEnvironmentRaw(env);
    try
      {
        return eval(ctx);
      }
    finally
      {
        if (env != save)
          ctx.setEnvironmentRaw(save);
      }
  }

  protected abstract boolean mustCompile ();

  public final int match0 (CallContext ctx)
  {
    ctx.proc = this;
    ctx.pc = 0;
    return 0;
  }

  public final Object apply0 () throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    check0(ctx);
    return ctx.runUntilValue();
  }

  /** Evaluate the expression.
   * This is named apply rather than eval so it is compatible with the
   * full-tail-call calling convention, and we can stash an Expression in
   * CallContext's proc field.  FIXME - are we making use of this?
   */
  public void apply (CallContext ctx) throws Throwable
  {
    throw new RuntimeException ("internal error - "
			        + getClass() + ".eval called");
  }

  public final void print (java.io.PrintWriter ps)
  {
    if (ps instanceof OutPort)
      print((OutPort) ps);
    else
      {
	OutPort out = new OutPort(ps);
	print(out);
	out.close();
      }
  }

  public abstract void print (OutPort ps);

  /**
   * Print line and column number if specified.
   * This is a helper routineintended for use by print(OutPort).
   */
  public void printLineColumn(OutPort out)
  {
    int line = getLine ();
    if (line > 0)
      {
	out.print("line:");
	out.print(line);
	int column = getColumn();
	if (column != 0)
	  {
	    out.print(':');
	    out.print(column);
	  }
	out.writeSpaceFill();
      }
  }

  public abstract void compile (Compilation comp, Target target);

  /** Same as compile, but emit line number beforehard. */
  public final void compileWithPosition(Compilation comp, Target target)
  {
    int line = getLine ();
    if (line > 0)
      {
        comp.getCode().putLineNumber(getFile(), line);
        compileNotePosition(comp, target, this);
      }
    else
      compile(comp, target);
  }

  /** Same as 2-argument compileWithPosition,
   * but use some other Expression's line number. */
  public final void compileWithPosition(Compilation comp, Target target,
					Expression position)
  {
    int line = position.getLine ();
    if (line > 0)
      {
        comp.getCode().putLineNumber(position.getFile(), line);
        compileNotePosition(comp, target, position);
      }
    else
      compile(comp, target);
  }

  /** Compile, but take note of line number. */
  public final void compileNotePosition(Compilation comp, Target target,
					Expression position)
  {
    String saveFilename = comp.getFile();
    int saveLine = comp.getLine();
    int saveColumn = comp.getColumn();
    comp.setLine(position);
    compile(comp, target);
    // This might logically belong in a `finally' clause.
    // It is intentionally not so, so if there is an internal error causing
    // an exception, we get the line number where the exception was thrown.
    comp.setLine(saveFilename, saveLine, saveColumn);
  }

  public final void compile (Compilation comp, Type type)
  {
    // Should we use Target.pushValue instead?  FIXME.
    compile (comp, StackTarget.getInstance(type));
  }

  /** Compile an expression with checking suitable for a known Declaration.
   * Leaves the result on the stack (i.e. does not assign to the lhs).
   * It does coerce the value to a suitable type for the lhs, and
   * throw a hopefully-informative WrongType exception on failure.
   */
  public final void compile (Compilation comp, Declaration lhs)
  {
    compile (comp,
             CheckedTarget.getInstance(lhs.getType(),
                                       lhs.getName(),
                                       WrongType.ARG_VARNAME));
  }

  /** Compile all but the first sub-"statement".
   * A kludge used for constructor methods, since if the first "statement"
   * is a super-constructor we need to inject initializer expressions. */
  public static void compileButFirst (Expression exp, Compilation comp)
  {
    if (exp instanceof BeginExp)
      {
        BeginExp bexp = (BeginExp) exp;
 	int n = bexp.length;
        if (n == 0)
          return;
        Expression[] exps = bexp.exps;
        compileButFirst(exps[0], comp);
	for (int i = 1; i < n; i++)
	  exps[i].compileWithPosition(comp, Target.Ignore);       
      }
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkExpression(this);
  }

  protected void walkChildren (ExpWalker walker) { }

  /** Apply inlining transformations on a given ApplyExp.
   * Assumes the ApplyExp's function is the this expression,
   * or can be optimized to this expression.
   * @param exp an application whose function expression can be simplified
   *  to this expression.
   * @param walker the context for the current inlining pass
   * @param decl if non-null, a Declaration bound to this expression
   * @return an Expression equivalent to te passed-in exp.
   */
  public Expression inline (ApplyExp exp, InlineCalls walker, Declaration decl)
  {
    return exp;
  }

  String filename;
  int position;

  public static final Expression[] noExpressions = new Expression[0];

  /** Helper method to create a `while' statement. */
  public static Expression makeWhile(Object cond, Object body, Compilation parser)
  {
    Expression[] inits = new Expression[1];
    LetExp let = new LetExp(inits);
    String fname = "%do%loop";
    Declaration fdecl = let.addDeclaration(fname);
    Expression recurse = new ApplyExp(new ReferenceExp(fdecl), noExpressions);
    LambdaExp lexp = new LambdaExp();
    parser.push(lexp);
    lexp.body = new IfExp(parser.parse(cond),
			  new BeginExp(parser.parse(body), recurse),
			  QuoteExp.voidExp);
    lexp.setName(fname);
    parser.pop(lexp);
    inits[0] = lexp;
    fdecl.noteValue(lexp);
    let.setBody(new ApplyExp(new ReferenceExp(fdecl), noExpressions));
    return let;
  }
  
  public final Expression setLine(Expression old)
  {
    this.filename = old.filename;
    this.position = old.position;
    return this;
  }

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    position = (lineno << 12) + colno;
  }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFile ()
  {
    return filename;
  }

  /** Set line number from current position in <code>Compilation</code>. */
  public void setLine (Compilation comp)
  {
    int line = comp.getLine();
    if (line != 0)
      {
	setFile(comp.getFile());
	setLine(line, comp.getColumn());
      }
  }

  /** Get the line number of (the start of) this Expression.
    * The "first" line is line 1. */
  public final int getLine ()
  {
    return position >> 12;
  }

  public final int getColumn ()
  {
    return position & ((1 << 12) - 1);
  }

  /** Return the Type used to represent the values of this Expression. */
  public Type getType()
  {
    return Type.pointer_type;
  }

  /** Return value if it is constant, or null if non-constant or unknown. */
  public Object valueIfConstant ()
  {
    return null;
  }

  protected int flags;
  protected static final int NEXT_AVAIL_FLAG = 1;

  public void setFlag (boolean setting, int flag)
  {
    if (setting) flags |= flag;
    else flags &= ~flag;
  }

  public void setFlag (int flag)
  {
    flags |= flag;
  }

  public int  getFlags()
  {
    return flags;
  }

  public boolean getFlag (int flag)
  {
    return (flags & flag) != 0;
  }

  /** True if evaluating may have side-effects. */
  public boolean side_effects () { return true; }

  public String toString ()
  {
    String tname = getClass().getName();
    if (tname.startsWith("gnu.expr."))
      tname = tname.substring(9);
    return tname+"@"+Integer.toHexString(hashCode());
  }
}
