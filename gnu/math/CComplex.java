// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;

/** General Cartesian Complex number.
 * Use this instead of DComplex if you want exact complex numbers.
 * @author	Per Bothner
 */

public class CComplex extends Complex
{
  RealNum real;
  RealNum imag;

  public CComplex (RealNum real, RealNum imag)
  {
    this.real = real;
    this.imag = imag;
  }

  public RealNum re() { return real; }
  public RealNum im() { return imag; }
}
