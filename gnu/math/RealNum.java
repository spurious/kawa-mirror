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

  public Numeric abs ()
  {
    return isNegative () ? neg () : this;
  }

  public boolean isZero ()
  {
    return sign () == 0;
  }
}
