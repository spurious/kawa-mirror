package gnu.math;

public abstract class UnsignedPrim extends Number {
    public long longValue() { return intValue(); }
    public float floatValue() { return (float) longValue(); }
    public double doubleValue() { return (double) longValue(); }

    @Override
    public int hashCode() { return intValue(); }

    public IntNum toIntNum() { return IntNum.valueOf(intValue()); }
    public abstract int numBits();
}
