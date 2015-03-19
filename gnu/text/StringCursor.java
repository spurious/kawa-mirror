package gnu.text;

import java.io.*;

/** Boxed class for the 'string-cursor' type. */

public final class StringCursor implements Comparable, Externalizable {
    int value;

    public static StringCursor valueOf(int value) {
        StringCursor sc = new StringCursor();
        sc.value = value;
        return sc;        
    }

    public int compareTo(Object o) {
        return value - ((StringCursor) o).value;
    }

    public static int checkStringCursor(Object obj) {
        return obj instanceof StringCursor ? ((StringCursor) obj).value : -2;
    }

    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeInt(value);
    }

    public void readExternal(ObjectInput in) throws IOException {
        value = in.readInt();
    }
}
