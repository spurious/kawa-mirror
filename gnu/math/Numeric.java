package kawa.math;

public abstract class Numeric
{
  public abstract Numeric add (Object obj);

  public abstract Numeric sub (Object obj);

  public abstract Numeric mul (Object obj);

  public abstract Numeric div (Object obj);

  public abstract Numeric abs ();

  public abstract Numeric neg ();

  public abstract String toString (int radix);

  public abstract boolean isExact ();

  public abstract boolean isZero ();

  public boolean equ (Object obj)
  {
    return sub (obj).isZero ();
  }

  /* Rounding modes: */
  public static final int FLOOR = 1;
  public static final int CEILING = 2;
  public static final int TRUNCATE = 3;
  public static final int ROUND = 4;

  /** Calculate x+this. */
  public Numeric add_reversed (Numeric x)
  {
    throw new IllegalArgumentException ();
  }

  /** Calculate x-this. */
  public Numeric sub_reversed (Numeric x)
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
}
