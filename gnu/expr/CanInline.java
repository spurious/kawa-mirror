package gnu.expr;

/** Inline an application, at the Expression level.
 * In contrast, Inlineable is done at code generation time.
 */

public interface CanInline
{
  /** Inline an application of this Procedure and return result.
   * This method is responsible for walking exp.getArgs();
   * you can handle this using {@code exp.getArgs(walker)}.
   * Can return original expression.
   */
  public Expression inline (ApplyExp exp, InlineCalls walker);
}

