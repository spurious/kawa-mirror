// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;

/** General Cartesian Complex quantity. */

public class CQuantity extends Quantity
{
  Complex num;
  Unit unt;

  public CQuantity (Complex num, Unit unit)
  {
    this.num = num;
    this.unt = unit;
  }

  public CQuantity (RealNum real, RealNum imag, Unit unit)
  {
    this.num = new CComplex (real, imag);
    this.unt = unit;
  }

  public Complex number() { return num; }
  public Unit unit() { return unt; }

  public boolean isExact () { return num.isExact(); }

  public boolean isZero () { return num.isZero(); }
}
