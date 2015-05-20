package gnu.math;

public class UShort extends UnsignedPrim implements Comparable<UShort> {
    short ival;
    public int numBits() { return 16; }

    public UShort(short ival) { this.ival = ival; }

    public static UShort valueOf(short ival) { return new UShort(ival); }

    public short shortValue() { return ival; }
    public int intValue() { return ival & 0xFFFF; }
    public long longValue() { return ival & 0xFFFF; }

    public IntNum toIntNum() { return IntNum.valueOf(ival & 0xFFFF); }

    public boolean equals(Object obj) {
        return obj instanceof UShort
            && ival == ((UShort) obj).ival;
    }

    public int compareTo(UShort other) {
        return intValue() - other.intValue();
    }

    public static String toString(short ival) {
        return Integer.toString(ival & 0xFFFF);
    }
    public String toString() { return toString(ival); }
}
