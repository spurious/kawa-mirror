package kawa.math;
import kawa.lang.*;

public abstract class RealNum extends Numeric
{
  public abstract double doubleValue ();

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

  public boolean equ (Object x)
  {
    return compare (x) == 0;
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

  public Numeric abs ()
  {
    return isNegative () ? neg () : this;
  }

  public boolean isZero ()
  {
    return sign () == 0;
  }
}
