package kawa.math;

/* A unit of measurement, either primitive (meter) or derived (kilogram).
 * @author	Per Bothner
 */

public class Unit extends Quantity
{
  String name;
  Dimensions dims;
  double factor;
  MulUnit products;
  Unit base;

  public final Dimensions dimensions() { return dims; }

  public final double doubleValue() { return factor; }

  public int hashCode () { return dims.hashCode (); }

  static Unit mul (Unit unit1, int power1, Unit unit2, int power2)
  {
    // First try various simplifications.
    if (unit1 == unit2)
      {
	power1 += power2;
	unit2 = Unit.Empty;
	power2 = 0;
      }
    if (power1 == 0)
      {
	unit1 = unit2;
	power1 = power2;
	unit2 = Unit.Empty;
	power2 = 0;
      }
    if (unit2 == Unit.Empty)
      {
	if (power1 == 1)
	  return unit1;
	if (power1 == 0)
	  return unit2;
      }
    if (unit1 instanceof MulUnit)
      {
	MulUnit munit1 = (MulUnit) unit1;
	if (munit1.unit1 == unit2)
	  return mul (unit2, munit1.power1 * power1 + power2,
		      munit1.unit2, munit1.power2 * power1);
	if (munit1.unit2 == unit2)
	  return mul (munit1.unit1, munit1.power1 * power1,
		      unit2, munit1.power2 * power1 + power2);
	if (unit2 instanceof MulUnit)
	  {
	    MulUnit munit2 = (MulUnit) unit2;
	    if (munit1.unit1 == munit2.unit1 && munit1.unit2 == munit2.unit2)
	      return mul (munit1.unit1,
			  munit1.power1 * power1 + munit2.power1 * power2,
			  munit1.unit2,
			  munit1.power2 * power1 + munit2.power2 * power2);
	    if (munit1.unit1 == munit2.unit2 && munit1.unit2 == munit2.unit1)
	      return mul (munit1.unit1,
			  munit1.power1 * power1 + munit2.power2 * power2,
			  munit1.unit2,
			  munit1.power2 * power1 + munit2.power1 * power2);
	  }
      }
    if (unit2 instanceof MulUnit)
      {
	MulUnit munit2 = (MulUnit) unit2;
	if (munit2.unit1 == unit1)
	  return mul (unit1, power1 + munit2.power1 * power2,
		      munit2.unit2, munit2.power2 * power2);
	if (munit2.unit2 == unit1)
	  return mul (munit2.unit1, munit2.power1 * power2,
		      unit1, power1 + munit2.power2 * power2);
      }

    // Search for an existing matching MulUnit.
    for (MulUnit u = unit1.products;  u != null;  u = u.next)
      {
	if (u.unit1 == unit1 && u.unit2 == unit2
	    && u.power1 == power1 && u.power2 == power2)
	  return u;
      }
    // Did not find a match - create one.
    return new MulUnit (unit1, power1, unit2, power2);
  }

  public static Unit mul (Unit unit1, Unit unit2)
  {
    return mul(unit1, 1, unit2, 1);
  }

  public static Unit div (Unit unit1, Unit unit2)
  {
    return mul(unit1, 1, unit2, -1);
  }

  public static Unit pow (Unit unit, int power)
  {
    return mul(unit, power, Unit.Empty, 0);
  }

  Unit ()
  {
    factor = 1.0;
  }

  public Unit (String name, double factor, Dimensions dims)
  {
    this.name = name;
    this.factor = factor;
    this.dims = dims;
    unitTable.put (name, this);
  }

  public Unit (String name, DQuantity value)
  {
    this (name, value.doubleValue(), value.unit().dimensions());
  }

  public Unit (String name, double factor, Unit base)
  {
    this (name, factor * base.doubleValue(), base.dimensions());
  }

  public Complex number() { return new DFloNum(factor); }
  public boolean isExact () { return false; }
  public final boolean isZero () { return false; }

  public Numeric power (IntNum y)
  {
    if (y.words != null)
      throw new ArithmeticException("Unit raised to bignum power");
    return pow (this, y.ival);
  }

  public static java.util.Hashtable unitTable = new java.util.Hashtable ();

  public static Unit Empty = new Unit ();
  static {
    Empty.dims = Dimensions.Empty;
  }

  public String toString (double val)
  {
    String str = Double.toString(val);
    if (this == Unit.Empty)
      return str;
    else
      return str + this.toString();
  }

  public String toString (RealNum val)
  {
    return toString (val.doubleValue());
  }

  public String toString (Complex val)
  {
    String str = toString(val.re());
    RealNum im = val.im();
    if (im.isZero())
      return str;
    return  str + "@" + toString(im);
  }

  public String toString ()
  {
    if (name != null)
      return name;
    else if (this == Unit.Empty)
      return "unit";
    else
      return Double.toString(factor) + "<unnamed unit>";
  }

  public Unit unit ()
  {
    if (base == null)
      {
	if (dims.bases[0] == Dimensions.endDummy)
	  base = Unit.Empty;
	else if (dims.powers[0] == 1 && dims.bases[1] == Dimensions.endDummy)
	  base = dims.bases[0];
	else
	  {
	    base = new Unit();
	    base.dims = dims;
	  }
      }
    return base;
  }

  public static Unit lookup (String name)
  {
    return (Unit) unitTable.get (name);
  }

  public static final BaseUnit meter = new BaseUnit ("m", "Length");
  public static final BaseUnit second = new BaseUnit ("s", "Time");
  public static final BaseUnit gram = new BaseUnit ("g", "Mass");
  public static final Unit cm = new Unit ("cm", 0.100, meter);
  public static final Unit mm = new Unit ("cm", 0.10, cm);
  public static final Unit in = new Unit ("in", 0.0254, meter);
  public static final Unit pt = new Unit ("pt", 0.0003527778, meter);
  public static final Unit pica = new Unit ("pica", 0.004233333, meter);

  public static final Unit minute = new Unit ("min", 60.0, second);
  public static final Unit hour = new Unit ("hour", 60.0, minute);
}
