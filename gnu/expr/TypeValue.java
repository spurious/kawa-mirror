// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/** A Type or a Type expression.
 * Can be used for higher-level types that do not map directly to a Type.
 */

public interface TypeValue
{
  /** The lower-level Type used to represent instances of this type. */
  public Type getImplementationType();

  /** Emit code for 'if (INCOMING instanceof THIS_TYPE) DECL = INCOMING ...'
   * This method is designed for 'typeswitch' applications, where this
   * call is the first part of a conditional, so it much be followed
   * by calls to emitElse and emitFi.
   * @param incoming Contains the value we are testing to see if it has the
   *        the type of 'this'.  If null, use top-of-stack.
   * @param decl If non-null, assign value after coercion to Declaration.
   * @param comp The compilation state.
   */
  public void emitTestIf(Variable incoming, Declaration decl,
			 Compilation comp);

  /** Emit code for 'INCOMING instanceof THIS_TYPE'.
   * @param incoming Contains the value we are testing to see if it has the
   *        the type of 'this'.  If null, use top-of-stack.
   * @param comp The compilation state.
   * @param target Where to leave the result.
   * @note  Implementation can use gnu.kawa.reflect.InstanceOf.emitIsInstance.
   */
  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target);
}
