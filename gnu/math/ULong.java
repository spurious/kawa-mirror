package gnu.math;

public class ULong extends UnsignedPrim implements Comparable<ULong> {
    long ival;
    public int numBits() { return 64; }

    public ULong(long ival) { this.ival = ival; }

    public static ULong valueOf(long ival) { return new ULong(ival); }

    public int intValue() { return (int) ival; }
    public long longValue() { return ival; }

    public IntNum toIntNum() { return IntNum.valueOfUnsigned(ival); }

    public boolean equals(Object obj) {
        return obj instanceof ULong
            && ival == ((ULong) obj).ival;
    }

    public int compareTo(ULong other) {
        /* #ifdef JAVA8 */
        // return Long.compareUnsigned(ival, other.ival);
        /* #else */
        long x = ival + Long.MIN_VALUE;
        long y = other.ival + Long.MIN_VALUE;
        return x < y ? -1 : x == y ? 0 : 1;
        /* #endif */
   }

    public static String toString(long ival) {
        if (ival >= 0)
            return Long.toString(ival);
        else
            return IntNum.valueOfUnsigned(ival).toString();
    }
    public String toString() { return toString(ival); }
}
