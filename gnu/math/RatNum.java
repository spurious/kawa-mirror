package kawa.math;
import kawa.lang.*;

/* The abstract class of rational numbers. */

public abstract class RatNum extends RealNum
{
  public abstract IntNum numerator ();
  public abstract IntNum denominator ();
}
