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

  public boolean isZero ()
  {
    return numerator().isZero();
  }

  /** Positive exact "rational" infinity. */
  public static RatNum Infinity
  = new IntFraction (IntNum.one (), IntNum.zero ());
  /** Negative exact "rational" infinity. */
  public static RatNum NegInfinity
  = new IntFraction (IntNum.make (-1), IntNum.zero ());

  public static int compare (RatNum x, RatNum y)
  {
    return IntNum.compare (IntNum.times (x.numerator (), y.denominator ()),
			   IntNum.times (y.numerator (), x.denominator ()));
  }

  /* Assumes x and y are both canonicalized. */
  public static boolean equals (RatNum x, RatNum y)
  {
    return IntNum.equals (x.numerator(), y.numerator())
      && IntNum.equals (x.denominator(), y.denominator());
  }

  /* Assumes this and obj are both canonicalized. */
  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof RatNum))
      return false;
    return RatNum.equals (this, (RatNum) obj);
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

  public static RatNum power (RatNum x, IntNum y)
  {
    int i = y.ival;
    if (y.words != null || i > 1000000 || i < -1000000)
      {
	IntNum xi;
	if (x instanceof IntNum && (xi = (IntNum) x).words == null)
	  {
	    i = xi.ival;
	    if (i == 0)
	      return y.isNegative () ? RatNum.Infinity : (RatNum) x;
	    if (i == 1)
	      return xi;
	    if (i == -1)
	      return y.isOdd () ? xi : IntNum.one ();
	  }
	throw new ArithmeticException ("exponent too big");
      }
    return power (x, i);
  }

  public static RatNum power (RatNum x, int i)
  {
    if (i == 0)
      return IntNum.one ();
    if (x.isZero ())
      return i < 0 ? RatNum.Infinity : (RatNum) x;
    if (i < 0)
      {
	i = -i;
	return RatNum.make (IntNum.power (x.denominator (), i),
			    IntNum.power (x.numerator (), i));
      }
    if (x instanceof IntNum)
      return IntNum.power ((IntNum) x, i);
    return RatNum.make (IntNum.power (x.numerator (), i),
			IntNum.power (x.denominator (), i));
  }

  public final RatNum toExact ()
  {
    return this;
  }

  public RealNum toInt (int rounding_mode)
  {
    // FIXME - divide (..., ROUND) not implemented for bignums!
    if (rounding_mode == ROUND)
      return super.toInt (ROUND);
    return IntNum.quotient (numerator(), denominator(), rounding_mode);
  }

  public IntNum toExactInt (int rounding_mode)
  {
    return IntNum.quotient (numerator(), denominator(), rounding_mode);
  }

  /** Calcaulte the simplest rational between two reals. */
  public static RealNum rationalize (RealNum x, RealNum y)
  {
    // This algorithm is copied from C-Gambit, copyright Marc Feeley.
    if (x.grt (y))
      return simplest_rational2 (y, x);
    else if (! (y.grt(x)))
      return x;
    else if (x.sign() > 0)
      return simplest_rational2 (x, y);
    else if (y.isNegative ())
      return (RealNum) (simplest_rational2 ((RealNum)y.neg(),
					    (RealNum)x.neg())).neg();
    else
      return IntNum.zero ();
  }
  private static RealNum simplest_rational2 (RealNum x, RealNum y)
  {
    RealNum fx = x.toInt (FLOOR);
    RealNum fy = y.toInt (FLOOR);
    if (! x.grt(fx))
      return fx;
    else if (fx.equ(fy))
      {
	RealNum n = (RealNum) IntNum.one().div(y.sub(fy));
	RealNum d = (RealNum) IntNum.one().div(x.sub(fx));
	return (RealNum) fx.add(IntNum.one().div(simplest_rational2 (n, d)));
      }
    else
      return (RealNum) fx.add(IntNum.one());
  }

  public String toString ()
  {
    return toString (10);
  }

}
