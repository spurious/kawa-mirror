// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;

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
    return ((RealNum)obj).compareReversed(this);
  }

  public int compareReversed (Numeric x)
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
    return ((Numeric)y).addReversed(this, k);
  }

  public Numeric addReversed (Numeric x, int k)
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
    return ((Numeric)y).mulReversed(this);
  }

  public Numeric mulReversed (Numeric x)
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
    return ((Numeric)y).divReversed(this);
  }

  public Numeric divReversed (Numeric x)
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

  public long longValue ()
  {
    return toExactInt (ROUND).longValue ();
  }

  public double doubleValue ()
  {
    boolean neg = num.isNegative ();
    IntNum n = num;
    if (neg)
      n = IntNum.neg (n);
    int num_len = n.intLength ();
    int den_len = den.intLength ();
    int exp = 0;
    if (num_len < den_len + 54)
      {
	exp = den_len + 54 - num_len;
	n = IntNum.shift (num, exp);
	exp = - exp;
      }

    // Divide n (which is shifted num) by den, using truncating division,
    // and return quot and remainder.
    IntNum quot = new IntNum ();
    IntNum remainder = new IntNum ();
    IntNum.divide (n, den, quot, remainder, TRUNCATE);
    quot = quot.canonicalize ();
    remainder = remainder.canonicalize ();

    return quot.roundToDouble (exp, neg, !remainder.isZero ());
  }

  public String toString (int radix)
  {
    return num.toString (radix) + '/' + den.toString ();
  }
}
