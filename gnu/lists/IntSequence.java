// Copyright (c) 2015  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.lists;

public interface IntSequence
    /* #ifdef JAVA8 */
    // extends java.util.function.IntUnaryOperator
    /* #endif */
{
    public int intAt(int index);

    public int size();

    /* #ifdef JAVA8 */
    // default int applyAsInt(int operand) {
    //     return intAt(operand);
    // }
    /* #endif */
}
