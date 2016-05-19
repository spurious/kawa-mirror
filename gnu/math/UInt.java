package gnu.math;

public class UInt extends UnsignedPrim implements Comparable<UInt> {
    int ival;
    public int numBits() { return 32; }

    public UInt(int ival) { this.ival = ival; }

    public static UInt valueOf(int ival) { return new UInt(ival); }

    public int intValue() { return ival; }
    public long longValue() { return (long) ival & 0xFFFFFFFFl; }

    public IntNum toIntNum() {
        return IntNum.valueOf(longValue());
    }

    public static String toString(int ival) {
        if (ival >= 0)
            return Integer.toString(ival);
        else
            return Long.toString(0xFFFFFFFFl & (long) ival);
    }

    public boolean equals(Object obj) {
        return obj instanceof UInt
            && ival == ((UInt) obj).ival;
    }

    public int compareTo(UInt other) {
        /* #ifdef JAVA8 */
        // return Integer.compareUnsigned(ival, other.ival);
        /* #else */
        int x = ival + Integer.MIN_VALUE;
        int y = other.ival + Integer.MIN_VALUE;
        return x < y ? -1 : x == y ? 0 : 1;
        /* #endif */
    }

    public String toString() { return toString(ival); }
}
