// Copyright (c) 2001, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;
import java.io.*;
import gnu.text.Char;

/** Simple adjustable-length vector whose elements are 16-bit chars.
 * Meant to be used as a wrapper for char arrays, so does not
 * implement CharSequence.
 * @author Per Bothner
 */

public class CharVector extends AbstractCharVector<Character>
{
    /** Create an CharVector from a char[].
     * Note that this contructor does *not* copy the argument. */
    public CharVector(char[] values) {
        data = values;
    }

    public final Character getRaw(int index) {
        return data[index];
    }

    @Override
    public final void setRaw(int index, Character value) {
        data[index] = value.charValue();
    }

    public boolean equals(Object obj) {
        return obj instanceof CharVector && equals(this, (CharVector) obj);
    }

   @Override
    protected CharVector newInstance(int newLength) {
        return new CharVector(newLength < 0 ? data : new char[newLength]);
    }

    public int getElementKind() { return CHAR_VALUE; }

    public String getTag() { return "c16"; }
}
