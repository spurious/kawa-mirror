package kawa.math;
import kawa.lang.*;

/** Implementation of exact rational numbers as fractions of IntNums.
 * @author Per Bothner
 */

public class IntFraction extends RatNum
{
  IntNum num;
  IntNum den;

  IntFraction (IntNum num, IntNum den)
  {
    this.num = num;
    this.den = den;
  }

  public final IntNum numerator () { return num; }
  public final IntNum denominator () { return den; }

  public final boolean isNegative () { return num.isNegative (); }

  public final int sign () { return num.sign (); }

  public final int compare (Object obj)
  {
    if (obj instanceof RatNum)
      return RatNum.compare (this, (RatNum) obj);
    if (! (obj instanceof RealNum))
      throw new IllegalArgumentException ();
    return ((RealNum)obj).compare_reversed (this);
  }

  public int compare_reversed (Numeric x)
  {
    if (!(x instanceof RatNum))
      throw new IllegalArgumentException ();
    return RatNum.compare ((RatNum) x, this);
  }

  public Numeric add (Object y, int k)
  {
    if (y instanceof RatNum)
      return RatNum.add (this, (RatNum) y, k);
    if (! (y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).add_reversed (this, k);
  }

  public Numeric add_reversed (Numeric x, int k)
  {
    if (! (x instanceof RatNum))
      throw new IllegalArgumentException ();
    return RatNum.add ((RatNum)x, this, k);
  }

  public Numeric mul (Object y)
  {
    if (y instanceof RatNum)
      return RatNum.times (this, (RatNum)y);
    if (! (y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).mul_reversed (this);
  }

  public Numeric mul_reversed (Numeric x)
  {
    if (! (x instanceof RatNum))
      throw new IllegalArgumentException ();
    return RatNum.times ((RatNum) x, this);
  }

  public Numeric div (Object y)
  {
    if (y instanceof RatNum)
      return RatNum.divide (this, (RatNum)y);
    if (! (y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).div_reversed (this);
  }

  public Numeric div_reversed (Numeric x)
  {
    if (! (x instanceof RatNum))
      throw new IllegalArgumentException ();
    return RatNum.divide ((RatNum)x, this);
  }

  public static IntFraction neg (IntFraction x)
  {
    // If x is normalized, we do not need to call RatNum.make to normalize.
    return new IntFraction (IntNum.neg (x.numerator()), x.denominator ());
  }

  public Numeric neg ()
  {
    return IntFraction.neg (this);
  }

  public double doubleValue ()
  {
    // FIXME.  Can cause unnecessary overflow or loss of precison.
    /// Better to calculate ((num << 64)/den).doubleValue() / (2**64);
    return numerator().doubleValue () / denominator().doubleValue();
  }

  public String toString (int radix)
  {
    return num.toString (radix) + '/' + den.toString ();
  }
}
