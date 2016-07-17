// Copyright (c) 2015  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.lists;

import java.util.List;

public interface IntSequence
    extends /* #ifdef JAVA8 */
            // java.util.function.IntUnaryOperator,
            /* #endif */
            AVector<Integer>
{
    public int getInt(int index);

    public int size();

    /* #ifdef JAVA8 */
    // default int applyAsInt(int operand) {
    //     return getInt(operand);
    // }
    /* #endif */
}
