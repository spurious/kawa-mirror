// Copyright (c) 1997  Cygnus Solutions, Inc.
// This is free software;  for terms and warranty disclaimer see ./LICENSE.

package gnu.bytecode;
import java.io.*;

/** The state of a conditional expression or statement. */

public class IfState {

  /** The surrounding IfState, if any. */
  IfState previous;

  /** True if we are curently in the else part of the conditional. */
  boolean doing_else;

  /** The (not-yet-defined) label at the end of the current sub-clause.
   * If doing_else, this is the end of the entire conditional;
   * otherwise, it is the end of the "then" clause. */
  Label end_label;

  /** The stack size before the "then" clause. */
  int start_stack_size;

  /** The types that were pushed by the then-clause. */
  Type[] then_stacked_types;

  public IfState (Method method)
  {
    previous = method.if_stack;
    method.if_stack = this;
    end_label = new Label (method);
  }
}

