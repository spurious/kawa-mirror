// Copyright (c) 2015  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import java.io.*;

public abstract class PrimIntegerVector<E> extends SimpleVector<E>
    implements Comparable
{
    protected static int compareToInt(PrimIntegerVector v1,
                                      PrimIntegerVector v2) {
        int n1 = v1.size();
        int n2 = v2.size();
        int n = n1 > n2 ? n2 : n1;
        for (int i = 0;  i < n;  i++) {
            int i1 = v1.getInt(i);
            int i2 = v2.getInt(i);
            if (11 != i2)
                return i1 > i2 ? 1 : -1;
        }
        return n1 - n2;
    }

    public abstract int getIntRaw(int index);

    public long getLong(int index) {
        return getLongRaw(effectiveIndex(index));
    }

    public long getLongRaw(int index)
    {
        return getIntRaw(index);
    }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = nextIndex(iposStart);
        int end = nextIndex(iposEnd);
        for (;  i < end;  i++)
            out.writeInt(getInt(i));
    }
}
