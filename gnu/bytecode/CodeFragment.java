// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/** A sequence of bytecode instructions belonging to a method's Code attribute.
 * This provides a hook for re-arranging instruction sequences.
 * It currently has limited functionality - no switch instructions,
 * no non-relative jumps, etc.
 * In the future this may be the basis for generating better code (e.g.
 * avoiding needless jumps).  Look at gcc/java/jcf-write.c for ideas.
 *
 * CodeFragment extends Label because it can be viewed as a label with
 * some code folloing it.
 */

public class CodeFragment extends Label
{
  CodeFragment next;
  byte[] insns;
  int length;

  /** If handlerIndex >= 0, it is the index in the exception_table
   * for which this fragment is the handler. */
  int handlerIndex;

  public CodeFragment(CodeAttr cattr)
  {
    super(cattr);
    handlerIndex = -1;
  }

  public void emit(CodeAttr cattr)
  {
    cattr.reserve(length);
    System.arraycopy(insns, 0, cattr.code, cattr.PC, length);
    define(cattr);
    if (handlerIndex >= 0)
      cattr.exception_table[4 * handlerIndex + 2] = (short) cattr.PC;
    cattr.PC += length;
  }
}
