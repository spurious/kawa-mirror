package gnu.lists;

import gnu.text.Char;
import java.io.*;

public abstract class AbstractCharVector<E>
    extends SimpleVector<E> implements Comparable
{
    public char[] data;
    protected static char[] empty = new char[0];

    public int length() { return size; }

    /** Get the allocated length of the data buffer. */
    public int getBufferLength() {
        return data.length;
    }

    public void setBufferLength(int length) {
        int oldLength = data.length;
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
            System.arraycopy(data, 0, d, 0, size);
            data = d;
        }
    }

    protected Object getBuffer() { return data; }

    public final char charAt(int index) {
        if (index >= size)
            throw new StringIndexOutOfBoundsException(index);
        return data[index];
    }

    public final char charAtBuffer(int index) {
        return data[index];
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
        int len = size;
        int hash = 0;
        for (int i = 0;  i < len;  i++)
            hash = 31 * hash + val[i];
        return hash;
    }

    /** Must override, since we override hashCode. */
    public abstract boolean equals(Object obj);

    public static boolean equals(AbstractCharVector<?> c1,
                                 AbstractCharVector<?> c2) {
        int len1 = c1.size;
        int len2 = c2.size;
        return len1 == len2 && compareTo(c1.data, c2.data, len1) == 0;
    }

    public int compareTo(Object obj) {
        AbstractCharVector<?> cv1 = this;
        AbstractCharVector<?> cv2 = (AbstractCharVector) obj;
        int n1 = cv1.size;
        int n2 = cv2.size;
        int n = n1 > n2 ? n2 : n1;
        int d = compareTo(cv1.data, cv2.data, n);
        return d != 0 ? d : n1 - n2;
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

    /**
     * @serialData Write 'size' (using writeInt),
     * followed by 'size' elements in order (using writeChar).
     */
    public void writeExternal(ObjectOutput out) throws IOException {
        int size = this.size;
        out.writeInt(size);
        char[] d = data; // Move to local to help optimizer.
        for (int i = 0;  i < size;  i++)
            out.writeChar(d[i]);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        int size = in.readInt();
        char[] data = new char[size];
        for (int i = 0;  i < size;  i++)
            data[i] = in.readChar();
        this.data = data;
        this.size = size;
    }
}
