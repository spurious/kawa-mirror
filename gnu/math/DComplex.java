package kawa.math;

/** A complex number using rectangular (Cartesian) plain double values.
 */

public class DComplex extends Complex
{ 
  double real;
  double imag;

  public DComplex (double real, double imag)
  {
    this.real = real;
    this.imag = imag;
  }

  public RealNum re () { return new DFloNum (real); }
  public double doubleValue() { return real; }
  public RealNum im () { return new DFloNum (imag); }
  public double doubleImagValue () { return imag; }

  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof Complex))
      return false;
    Complex y = (Complex)obj;
    return y.unit() == Unit.Empty
      && (Double.doubleToLongBits(real)
	  == Double.doubleToLongBits(y.reValue()))
      && (Double.doubleToLongBits(imag)
	  == Double.doubleToLongBits(y.imValue()));
  }

  // All transcendental complex functions return DComplex

  public final Numeric neg () { return new DComplex (-real, -imag); }

  public Numeric add (Object y, int k)
  {
    if (y instanceof Complex)
      {
        Complex yc = (Complex)y;
	if (yc.dimensions() != Dimensions.Empty)
	  throw new ArithmeticException ("units mis-match");
	return new DComplex (real + k * yc.reValue(),
			     imag + k * yc.imValue());
      }
    return ((Numeric)y).add_reversed (this, k);
  }

  public Numeric mul (Object y)
  {
    if (y instanceof Complex)
      {
        Complex yc = (Complex)y;
	if (yc.unit() == Unit.Empty)
	  {
	    double y_re = yc.reValue();
	    double y_im = yc.imValue();
	    return new DComplex (real * y_re - imag * y_im,
				 real * y_im + imag * y_re);
	  }
	return Complex.mul (this, yc);
      }
    return ((Numeric)y).mul_reversed (this);
  }

}
