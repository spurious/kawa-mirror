// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./LICENSE.

package gnu.math;

public abstract class Complex extends Quantity
{
  public Complex number() { return this; }

  public boolean isExact ()
  {
    // Should we return false if unit() != unit.Empty ?
    return re().isExact() && im().isExact();
  }

  public static CComplex imOne = new CComplex (IntNum.zero(), IntNum.one());

  public double doubleValue () { return re().doubleValue (); }
  public double doubleImagValue () { return im().doubleValue (); }
  public final double doubleRealValue () { return doubleValue (); }
  public long longValue () { return re().longValue(); }

  public static Complex make (RealNum re, RealNum im)
  {
    if (im.isZero ())
      return re;
    if (! re.isExact() || ! im.isExact())
      return new DComplex(re.doubleValue(), im.doubleValue());
    return new CComplex (re, im);
  }

  public static Complex make (double re, double im)
  {
    if (im == 0.0)
      return new DFloNum(re);
    return new DComplex(re, im);
  }

  public static DComplex polar (double r, double t)
  {
    return new DComplex(r * Math.cos(t), r * Math.sin(t));
  }

  public static DComplex polar (RealNum r, RealNum t)
  {
    return polar(r.doubleValue(), t.doubleValue());
  }

  public static Complex power (Complex x, Complex y)
  {
    if (y instanceof IntNum)
      return (Complex) x.power((IntNum) y);
    double x_re = x.doubleRealValue();
    double x_im = x.doubleImagValue();
    double y_re = y.doubleRealValue();
    double y_im = y.doubleImagValue();
    if (x_im == 0.0 && y_im == 0
	&& (x_re >= 0 || Double.isInfinite(x_re) || Double.isNaN(x_re)))
      return new DFloNum (Math.pow (x_re, y_re));
    return DComplex.power (x_re, x_im, y_re, y_im);
  }

  public Numeric abs ()
  {  
    return new DFloNum(DComplex.hypot(doubleRealValue(), doubleImagValue()));
  }

  public RealNum angle()
  {
    return new DFloNum(Math.atan2(doubleImagValue(), doubleRealValue()));
  }

  public static boolean equals (Complex x, Complex y)
  {
    return x.re().equals(y.re())
      && x.im().equals(x.im());
  }

  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof Complex))
      return false;
    return Complex.equals (this, (Complex) obj);
  }

  public static int compare (Complex x, Complex y)
  {
    int code = x.im().compare(y.im());
    if (code != 0)
      return code;
    return x.re().compare(y.re());
  }

  public int compare (Object obj)
  {
    if (! (obj instanceof Complex))
      return ((Numeric) obj).compare_reversed(this);
    return compare(this, (Complex) obj);
  }

  public boolean isZero ()
  {
    return re().isZero () && im().isZero();
  }

  //  public abstract Complex neg ();

  /*
  Unit unit () { return Unit.Empty; }
  Dimesions dims() { return unit().dims; }
  */

  
  public String toString (int radix)
  {
    String reString = re().toString(radix);
    String imString = im().toString(radix);
    if (imString.equals("0"))
      return reString;
    imString = imString + "i";
    if (reString.equals("0"))
      return imString;
    if (imString.charAt(0) != '-')
      imString = "+" + imString;
    return reString + imString;
  }

  public static Complex neg (Complex x)
  {
    return Complex.make (x.re().rneg(), x.im().rneg());
  }

  public Numeric neg () { return neg (this); }

  public static Complex add (Complex x, Complex y, int k)
  {
    return Complex.make (RealNum.add(x.re(), y.re(), k),
			 RealNum.add(x.im(), y.im(), k));
  }

  public Numeric add (Object y, int k)
  {
    if (y instanceof Complex)
      return add (this, (Complex) y, k);
    return ((Numeric)y).add_reversed (this, k);
  }

  public Numeric add_reversed (Numeric x, int k)
  {
    if (x instanceof Complex)
      return add ((Complex)x, this, k);
    throw new IllegalArgumentException ();
  }

  public static Complex mul (Complex x, Complex y)
  {
    RealNum x_re = x.re();
    RealNum x_im = x.im();
    RealNum y_re = y.re();
    RealNum y_im = y.im();
    return Complex.make (RealNum.add (RealNum.mul(x_re, y_re),
				      RealNum.mul(x_im, y_im), -1),
			 RealNum.add (RealNum.mul(x_re, y_im),
				      RealNum.mul(x_im, y_re), 1));
  }

  public Numeric mul (Object y)
  {
    if (y instanceof Complex)
      return mul (this, (Complex) y);
    return ((Numeric)y).mul_reversed (this);
  }

  public Numeric mul_reversed (Numeric x)
  {
    if (x instanceof Complex)
      return mul ((Complex)x, this);
    throw new IllegalArgumentException ();
  }

  public static Complex div (Complex x, Complex y)
  {
    if (! x.isExact () || ! y.isExact ())
      return DComplex.div (x.doubleRealValue(), x.doubleImagValue(),
			   y.doubleRealValue(), y.doubleImagValue());

    RealNum x_re = x.re();
    RealNum x_im = x.im();
    RealNum y_re = y.re();
    RealNum y_im = y.im();

    RealNum q = RealNum.add (RealNum.mul(y_re, y_re),
			     RealNum.mul(y_im, y_im), 1);
    RealNum n = RealNum.add(RealNum.mul(x_re, y_re),
			    RealNum.mul(x_im, y_im), 1);
    RealNum d = RealNum.add(RealNum.mul(x_im, y_re),
			    RealNum.mul(x_re, y_im), -1);
    return Complex.make(RealNum.div(n, q), RealNum.div(d, q));
  }

  public Numeric div (Object y)
  {
    if (y instanceof Complex)
      return div (this, (Complex) y);
    return ((Numeric)y).div_reversed (this);
  }

  public Numeric div_reversed (Numeric x)
  {
    if (x instanceof Complex)
      return div ((Complex)x, this);
    throw new IllegalArgumentException ();
  }

  public Complex exp ()
  {
    return polar (Math.exp(doubleRealValue()), doubleImagValue());
  }


  public Complex log ()
  {
    return DComplex.log(doubleRealValue(), doubleImagValue());
  }
  
  public Complex sqrt ()
  {
    return DComplex.sqrt(doubleRealValue(), doubleImagValue());
  }
}
