package kawa.math;
import kawa.lang.*;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;
import codegen.Type;

public class DFloNum extends RealNum // implements Compilable
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
    throw new IllegalArgumentException ();
  }

  public Numeric sub (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (value - ((RealNum)y).doubleValue ());
    throw new IllegalArgumentException ();
  }

  public Numeric mul (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (value * ((RealNum)y).doubleValue ());
    throw new IllegalArgumentException ();
  }

  public Numeric div (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (value / ((RealNum)y).doubleValue ());
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
    throw new IllegalArgumentException ();
  }

  public boolean isExact ()
  {
    return false;
  }

  public boolean isZero ()
  {
    return value == 0.0;
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

}
