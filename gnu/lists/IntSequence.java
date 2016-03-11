// Copyright (c) 2015  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.lists;

import java.util.List;

public interface IntSequence
    extends Array<Integer>,
            /* #ifdef JAVA8 */
            // extends java.util.function.IntUnaryOperator,
            /* #endif */
            List<Integer>
{
    public int getInt(int index);

    public int size();

    /* #ifdef JAVA8 */
    // default int applyAsInt(int operand) {
    //     return getInt(operand);
    // }
    /* #endif */
}
