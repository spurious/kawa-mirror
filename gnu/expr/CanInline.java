package gnu.expr;

/** Inline an application, at the Expression level.
 * In contrast, Inlineable is done at code generation time.
 */

public interface CanInline
{
  /** Inline an application of this Procedure and return result.
   * Unless {@code argsInlined} is true, then this method
   * is responsible for walking {@code exp.getArgs()};
   * you can handle this using {@code exp.getArgs(walker, argsInlined)}.
   * (The {@code exp.getFunction()} has been walked, regardless.)
   * @param argsInlined true if the arguments have been inlined.
   * Can return original expression.
   */
  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined);
}

