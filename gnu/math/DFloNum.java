package kawa.math;
import kawa.lang.*;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;
import codegen.Type;

public class DFloNum extends RealNum implements Compilable
{
  double value;

  public DFloNum (double value)
  {
    this.value = value;
  }

  public DFloNum (String s) throws NumberFormatException
  {
    Double d = new Double (s); // wasteful ...
    value = d.doubleValue ();
  }

  public static DFloNum make (double value)
  {
    return new DFloNum (value);
  }

  public double doubleValue ()
  {
    return value;
  }

  public int hashCode ()
  {
    return (int)value;
  }

  public boolean equals (Object obj)
  {
    // take from java.lang.Double.equals:
    return (obj != null)
      && (obj instanceof DFloNum) 
      && (Double.doubleToLongBits(((DFloNum)obj).value)
	  == Double.doubleToLongBits(value));
  }

  public Numeric add (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (value + ((RealNum)y).doubleValue ());
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).add_reversed (this);
  }

  public Numeric add_reversed (Numeric x)
  {
    if (x instanceof RealNum)
      return new DFloNum (((RealNum)x).doubleValue () + value);
    throw new IllegalArgumentException ();
  }

  public Numeric sub (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (value - ((RealNum)y).doubleValue ());
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).sub_reversed (this);
  }

  public Numeric sub_reversed (Numeric x)
  {
    if (x instanceof RealNum)
      return new DFloNum (((RealNum)x).doubleValue () - value);
    throw new IllegalArgumentException ();
  }

  public Numeric mul (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (value * ((RealNum)y).doubleValue ());
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).mul_reversed (this);
  }

  public Numeric mul_reversed (Numeric x)
  {
    if (x instanceof RealNum)
      return new DFloNum (((RealNum)x).doubleValue () * value);
    throw new IllegalArgumentException ();
  }

  public Numeric div (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (value / ((RealNum)y).doubleValue ());
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).div_reversed (this);
  }

  public Numeric div_reversed (Numeric x)
  {
    if (x instanceof RealNum)
      return new DFloNum (((RealNum)x).doubleValue () / value);
    throw new IllegalArgumentException ();
  }

  public boolean isNegative ()
  {
    return value >= 0;
  }

  public Numeric neg ()
  {
    return new DFloNum (-value);
  }

  public int sign ()
  {
    return value > 0.0 ? 1 : value < 0.0 ? -1 : value == 0.0 ? 0: -2;
  }

  public static int compare (double x, double y)
  {
    return x > y ? 1 : x < y ? -1 : x == y ? 0 : -2;
  }

  public int compare (Object obj)
  {
    if (obj instanceof RealNum)
      return compare (value, ((RealNum)obj).doubleValue ());
    if (! (obj instanceof RealNum))
      throw new IllegalArgumentException ();
    return ((RealNum)obj).compare_reversed (this);
  }

  public int compare_reversed (Numeric x)
  {
    if (!(x instanceof RealNum))
      throw new IllegalArgumentException ();
    return compare (((RealNum)x).doubleValue (), value);
  }

  public boolean isExact ()
  {
    return false;
  }

  public boolean isZero ()
  {
    return value == 0.0;
  }

  /** Converts to the closest exact rational value. */
  public static RatNum toExact (double value)
  {
    if (Double.isInfinite (value))
      return value >= 0.0 ? RatNum.Infinity : RatNum.NegInfinity;
    if (Double.isNaN (value))
      throw new ArithmeticException ("cannot convert NaN to exact rational");
    long bits = Double.doubleToLongBits (value);
    boolean neg = bits < 0;
    int exp = (int) (bits >> 52) & 0x7FF;
    bits &= 0xfffffffffffffL;
    if (exp == 0)
      bits <<= 1;
    else
      bits |= 0x10000000000000L;
    IntNum two = IntNum.make(2);
    RatNum result;
    if (exp >= 1075)
      result = IntNum.power (two, exp - 1075);
    else
      result = new IntFraction (IntNum.one(), IntNum.power (two, 1075 - exp));
    return RatNum.times (IntNum.make (neg ? -bits : bits), result);
  }

  public String toString ()
  {
    return Double.toString (value);
  }
  public String toString (int radix)
  {
    // ignore radix - FIXME
    return Double.toString (value);
  }

  static ClassType thisType;
  static Method makeMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (thisType == null)
      {
	thisType = new ClassType ("kawa.math.DFloNum");
	Type[] args = new Type[1];
	args[0] = Type.double_type;
	makeMethod = thisType.new_method ("make", args, thisType,
					     Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, thisType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    comp.method.compile_push_double (value);
    comp.method.compile_invoke_static (makeMethod);
  }
}
