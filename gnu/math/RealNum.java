package kawa.math;
import kawa.lang.*;

public abstract class RealNum extends Complex
{
  public final RealNum re() { return this; }
  public final RealNum im() { return IntNum.zero(); }

  public abstract boolean isNegative ();

  /** Return 1 if >0; 0 if ==0; -1 if <0; -2 if NaN. */
  public abstract int sign ();

  /** Return 1 if this>obj; 0 if this==obj; -1 if this<obj;
   * -2 if either is NaN. */
  public abstract int compare (Object obj);

  public int compare_reversed (Numeric x)
  {
    throw new IllegalArgumentException ();
  }

  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof RealNum))
      return false;
    return compare (obj) == 0;
  }

  public boolean grt (Object x)
  {
    return compare (x) > 0;
  }

  public boolean geq (Object x)
  {
    return compare (x) >= 0;
  }

  public RealNum max (RealNum x)
  {
    boolean exact = isExact () && x.isExact ();
    RealNum result = grt (x) ? this : x;
    if (!exact && result.isExact ())
      result = new DFloNum (result.doubleValue ());
    return result;
  }

  public RealNum min (RealNum x)
  {
    boolean exact = isExact () && x.isExact ();
    RealNum result = grt (x) ? x : this;
    if (!exact && result.isExact ())
      result = new DFloNum (result.doubleValue ());
    return result;
  }

  public static RealNum add (RealNum x, RealNum y, int k)
  {
    return (RealNum)(x.add(y, k));
  }

  public static RealNum mul (RealNum x, RealNum y)
  {
    return (RealNum)(x.mul(y));
  }

  public static RealNum div (RealNum x, RealNum y)
  {
    return (RealNum)(x.div(y));
  }

  /* These are defined in Complex, but have to be overridden. */
  public abstract Numeric add (Object obj, int k);
  public abstract Numeric mul (Object obj);
  public abstract Numeric div (Object obj);

  public RealNum angle()
  {
    return IntNum.zero();
  }

  public Numeric abs ()
  {
    return isNegative () ? neg () : this;
  }

  public final RealNum rneg() { return (RealNum) neg(); }

  public boolean isZero ()
  {
    return sign () == 0;
  }

  /** Convert to an exact number.
   * Implements the Scheme inexact->exact (for real numbers).
   */
  public RatNum toExact ()
  {
    return DFloNum.toExact (doubleValue ());
  }

  /** Converts a real to an integer, according to a specified rounding mode.
   * Note an inexact argument gives an inexact result, following Scheme.
   * See also RatNum.toExactInt. */
  public RealNum toInt (int rounding_mode)
  {
    double d = doubleValue ();
    switch (rounding_mode)
      {
      case FLOOR:
	d = Math.floor (d);
	break;
      case CEILING:
	d = Math.ceil (d);
	break;
      case TRUNCATE:
	d = d < 0.0 ? Math.ceil (d) : Math.floor (d);
	break;
      case ROUND:
	d -= Math.IEEEremainder (d, 1.0);
	break;
      }
    return new DFloNum (d);
  }

  public Complex exp ()
  {
    return new DFloNum(Math.exp(doubleValue()));
  }

  public Complex log ()
  {
    double x = doubleValue();
    if (x < 0)
      return DComplex.log(x, 0.0);
    return new DFloNum(Math.log(x));
  }

  public final Complex sin() { return new DFloNum(Math.sin(doubleValue())); }

  public final Complex sqrt ()
  {
    double d = doubleValue();
    if (d >= 0)
      return new DFloNum(Math.sqrt(d));
    else
      return DComplex.sqrt(d, 0);
  }
}
