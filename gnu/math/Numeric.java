package kawa.math;

public abstract class Numeric
{
  /** Return this + k * obj. */
  public abstract Numeric add (Object obj, int k);

  public final Numeric add (Object obj) { return add (obj, 1); }
  public Numeric sub (Object obj) { return add (obj, -1); }

  public abstract Numeric mul (Object obj);

  public abstract Numeric div (Object obj);

  public abstract Numeric abs ();

  public abstract Numeric neg ();

  public abstract String toString (int radix);

  public String toString () { return toString (10); }

  public abstract boolean isExact ();

  public abstract boolean isZero ();

  /* Rounding modes: */
  public static final int FLOOR = 1;
  public static final int CEILING = 2;
  public static final int TRUNCATE = 3;
  public static final int ROUND = 4;

  /** Calculate x+k&this. */
  public Numeric add_reversed (Numeric x, int k)
  {
    throw new IllegalArgumentException ();
  }

  public Numeric mul_reversed (Numeric x)
  {
    throw new IllegalArgumentException ();
  }

  public Numeric div_reversed (Numeric x)
  {
    throw new IllegalArgumentException ();
  }

  /** Return the multiplicative inverse. */
  public Numeric div_inv ()
  {
    return IntNum.one().div(this);
  }

  /** Return the multiplicative identity. */
  public Numeric mul_ident ()
  {
    return IntNum.one();
  }

  /** Return this raised to an integer power.
   * Implemented by repeated squaring and multiplication.
   * If y < 0, returns div_inv of the result. */
  public Numeric power (IntNum y)
  {
    if (y.isNegative ())
      return power(IntNum.neg(y)).div_inv();
    Numeric pow2 = this;
    Numeric r = null;
    for (;;)  // for (i = 0;  ; i++)
      {
	// pow2 == x**(2**i)
	// prod = x**(sum(j=0..i-1, (y>>j)&1))
	if (y.isOdd())
	  r = r == null ? pow2 : r.mul (pow2);  // r *= pow2
	y = IntNum.shift (y, -1);
	if (y.isZero())
	  break;
	// pow2 *= pow2;
	pow2 = pow2.mul (pow2);
      }
    return r == null ? mul_ident() : r;
  }

}
