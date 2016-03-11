package gnu.lists;

import gnu.text.Char;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.InPort;
import java.io.*;

public abstract class AbstractCharVector<E>
    extends SimpleVector<E> implements Comparable
{
    protected char[] data;
    protected static char[] empty = new char[0];

    public int length() { return size(); }

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void copyBuffer(int length) {
        int oldLength = data.length;
        if (length == -1)
            length = oldLength;
        if (oldLength != length) {
            char[] tmp = new char[length];
            System.arraycopy(data, 0, tmp, 0,
                             oldLength < length ? oldLength : length);
            data = tmp;
        }
    }

    public void ensureBufferLength (int sz) {
        if (sz > data.length) {
            char[] d = new char[sz < 60 ? 120 : 2 * sz];
            System.arraycopy(data, 0, d, 0, data.length);
            data = d;
        }
    }

    @Override
    public char[] getBuffer() { return data; }

    protected void setBuffer(Object buffer) { data = (char[]) buffer; }

    public final char charAt(int index) {
        return data[effectiveIndex(index)];
    }

    public final char getCharRaw(int index) {
        return data[index];
    }

    /** Copy characters into a destination buffer.
     * Same interface as java.lang.String's getChars. */
    public void getChars(int srcBegin, int srcEnd, char dst[], int dstBegin) {
        if (srcBegin < 0 || srcBegin > srcEnd)
            throw new StringIndexOutOfBoundsException(srcBegin);
        int size = size();
        if (srcEnd > size)
            throw new StringIndexOutOfBoundsException(srcEnd);
        if (dstBegin+srcEnd-srcBegin > dst.length)
            throw new StringIndexOutOfBoundsException(dstBegin);
        int len = srcEnd - srcBegin;
        if (len <= 0)
            return;
        if (isVerySimple())
            System.arraycopy(data, srcBegin, dst, dstBegin, len);
        else {
            for (int i = 0; i < len;  i++)
                dst[dstBegin+i] = charAt(srcBegin+i);
        }
  }

    protected void clearBuffer(int start, int count) {
        char[] d = data; // Move to local to help optimizer.
        while (--count >= 0)
            d[start++] = 0;
    }

    public int hashCode() {
        /* Matches String.hashCode specification, as updated specification in
           http://www.javasoft.com/docs/books/jls/clarify.html. */
        char[] val = data;
        int len = size();
        int hash = 0;
        if (! isVerySimple()) {
            for (int i = 0;  i < len;  i++)
                hash = 31 * hash + val[effectiveIndex(i)];
        } else {
            for (int i = 0;  i < len;  i++)
                hash = 31 * hash + val[i];
        }
        return hash;
    }

    /** Must override, since we override hashCode. */
    public abstract boolean equals(Object obj);

    public static boolean equals(AbstractCharVector<?> c1,
                                 AbstractCharVector<?> c2) {
        int len1 = c1.size();
        int len2 = c2.size();
        return len1 == len2 && compareTo(c1.data, c2.data, len1) == 0;
    }

    public int compareTo(Object obj) {
        AbstractCharVector<?> cv1 = this;
        AbstractCharVector<?> cv2 = (AbstractCharVector) obj;
        int n1 = cv1.size();
        int n2 = cv2.size();
        int n = n1 > n2 ? n2 : n1;
        int d = compareTo(cv1, cv2, n);
        return d != 0 ? d : n1 - n2;
    }

    public static int compareTo(AbstractCharVector<?> cv1,
                                AbstractCharVector<?> cv2,
                                int length) {
        // Needlessly conservative - could use the 'else' case also
        // if both indexes are simple ranges starting at 0 stepping by 1.
        if (! cv1.isVerySimple() || ! cv2.isVerySimple()) {
            for (int i = 0; i < length; i++) {
                char c1 = cv1.charAt(i);
                char c2 = cv2.charAt(i);
                int d = c1 - c2;
                if (d != 0)
                    return d;
            }
            return 0;
        } else {
            return compareTo(cv1.data, cv2.data, length);
        }
    }

    public static int compareTo(char[] arr1, char[] arr2, int length) {
        for (int i = 0; i < length; i++) {
            char c1 = arr1[i];
            char c2 = arr2[i];
            int d = c1 - c2;
            if (d != 0)
                return d;
        }
        return 0;
    }

    public CharArrayInPort openReader() {
        return new CharArrayInPort(this, data, 0, size());
    }

    public CharArrayInPort openReader(int start, int end) {
        return new CharArrayInPort(this, data, start, end);
    }
}
