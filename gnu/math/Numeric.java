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

  /*
  public abstract Object add_reversed (Numeric obj);

  public abstract Object mul_reversed (Numeric obj);

  public abstract Numeric zero ();
  */
}
