package kawa.math;

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

  /*
  public RealNum abs ()
  {
    Double real = re().as_double ();
    Double imag = im().as_double ();
    return Math.sqrt (real * real + imag * imag);
  }
  */

  public static boolean equals (Complex x, Complex y)
  {
    return x.re().equals(y.re())
      && x.im().equals(x.im())
      && x.unit().equals(y.unit());
  }

  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof Complex))
      return false;
    return Complex.equals (this, (Complex) obj);
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

  public Numeric div (Object y)
  {
    throw new Error ("Complex.div not implemented yet!");   //FIXME!
  }

  public Numeric abs ()
  {
    throw new Error ("Complex.abs not implemented yet!");   //FIXME!
  }


}
