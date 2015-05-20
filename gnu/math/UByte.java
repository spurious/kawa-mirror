package gnu.math;

public class UByte extends UnsignedPrim implements Comparable<UByte> {
    byte ival;
    public int numBits() { return 8; }

    public UByte(byte ival) { this.ival = ival; }

    public static UByte valueOf(byte ival) { return new UByte(ival); }

    public int intValue() { return ival & 0xFF; }

    public IntNum toIntNum() { return IntNum.valueOf(ival & 0xFFFF); }

    public boolean equals(Object obj) {
        return obj instanceof UByte
            && ival == ((UByte) obj).ival;
    }

    public int compareTo(UByte other) {
        return intValue() - other.intValue();
    }

    public static String toString(byte ival) {
        return Integer.toString(ival & 0xFF);
    }
    public String toString() { return toString(ival); }
}
