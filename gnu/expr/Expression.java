package gnu.expr;
import gnu.bytecode.Type;
import gnu.mapping.*;

/**
 * Abstract class for syntactic forms that evaluate to a value.
 * Scheme S-expressions get re-written to these before evaluation.
 * @author	Per Bothner
 */

public abstract class Expression implements Printable
{
  public Object eval (Environment env)
  {
    throw new RuntimeException ("internal error - "
			        + getClass() + ".eval called");
  }

  abstract public void print (java.io.PrintWriter ps);

  public abstract void compile (Compilation comp, Target target);

  /** Same as compile, but emit line number beforehard. */
  public final void compileWithPosition(Compilation comp, Target target)
  {
    int line = getLine ();
    if (line > 0)
      {
        comp.method.compile_linenumber (line);
        compileNotePosition(comp, target);
      }
    else
      compile(comp, target);
  }

  /** Compile, but take note of line number. */
  public final void compileNotePosition(Compilation comp, Target target)
  {
    String saveFilename = comp.filename;
    int savePosition = comp.position;
    comp.filename = filename;
    comp.position = position;
    compile(comp, target);
    // This might logically belong in a `finally' clause.
    // It is intentionally not so, so if there is an internal error causing
    // an exception, we get the line number where the exception was thrown.
    comp.filename = saveFilename;
    comp.position = savePosition;
  }

  public final void compile (Compilation comp, Type type)
  {
    // Should we use Target.pushValue instead?  FIXME.
    compile (comp, StackTarget.getInstance(type));
  }

  abstract Object walk (ExpWalker walker);

  String filename;
  int position;

  public static final Expression[] noExpressions = new Expression[0];

  /** Helper method to create a `while' statement. */
  public static Expression makeWhile(Object cond, Object body, Parser parser)
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


}
