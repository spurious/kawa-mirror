package kawa.math;
import kawa.lang.*;

/* The abstract class of rational numbers. */

public abstract class RatNum extends RealNum
{
  public abstract IntNum numerator ();
  public abstract IntNum denominator ();

  public static RatNum make(IntNum num, IntNum den)
  {
    IntNum g = IntNum.gcd (num, den);
    if (den.isNegative ())
      g = IntNum.neg (g);
    if (! g.isOne ())
      {
	num = IntNum.quotient (num, g);
	den = IntNum.quotient (den, g);
      }
    return den.isOne () ? (RatNum)num : (RatNum)(new IntFraction (num, den));
  }

  public boolean isExact ()
  {
    return true;
  }

  public static int compare (RatNum x, RatNum y)
  {
    return IntNum.compare (IntNum.times (x.numerator (), y.denominator ()),
			   IntNum.times (y.numerator (), x.denominator ()));
  }

  public static RatNum plus (RatNum x, RatNum y)
  {
    IntNum x_num = x.numerator();
    IntNum x_den = x.denominator();
    IntNum y_num = y.numerator();
    IntNum y_den = y.denominator();
    if (IntNum.equals (x_den, y_den))
      return RatNum.make (IntNum.plus (x_num, y_num), x_den);
    return RatNum.make (IntNum.plus (IntNum.times (y_den, x_num),
				     IntNum.times (y_num, x_den)),
			IntNum.times (x_den, y_den));
  }

  public static RatNum minus (RatNum x, RatNum y)
  {
    IntNum x_num = x.numerator();
    IntNum x_den = x.denominator();
    IntNum y_num = y.numerator();
    IntNum y_den = y.denominator();
    if (IntNum.equals (x_den, y_den))
      return RatNum.make (IntNum.minus (x_num, y_num), x_den);
    return RatNum.make (IntNum.minus (IntNum.times (y_den, x_num),
				     IntNum.times (y_num, x_den)),
			IntNum.times (x_den, y_den));
  }

  public static RatNum times (RatNum x, RatNum y)
  {
    return RatNum.make (IntNum.times (x.numerator(), y.numerator()),
			IntNum.times (x.denominator(), y.denominator()));
  }

  public static RatNum divide (RatNum x, RatNum y)
  {
    return RatNum.make (IntNum.times (x.numerator(), y.denominator()),
			IntNum.times (x.denominator(), y.numerator()));
  }

  public String toString ()
  {
    return toString (10);
  }

}
