package kawa.math;

/** A Unit which is the product or ratio of two other Units.
 * @author	Per Bothner
 */

class MulUnit extends Unit
{
  Unit unit1;
  Unit unit2;
  int power1;
  int power2;
  MulUnit next;

  MulUnit (Unit unit1, int power1, Unit unit2, int power2)
  {
    this.unit1 = unit1;
    this.unit2 = unit2;
    this.power1 = power1;
    this.power2 = power2;
    this.dims = Dimensions.product (unit1.dims, power1, unit2.dims, power2);

    if (power1 == 1)
      factor = unit1.factor;
    else
      factor = Math.pow (unit1.factor, (double) power1);
    if (power2 < 0)
      {
	for (int i = -power2;  --i >= 0; )
	  factor /= unit2.factor;
      }
    else
      {
	for (int i = power2;  --i >= 0; )
	  factor *= unit2.factor;
      }

    next = unit1.products;
    unit1.products = this;
  }

  MulUnit (Unit unit1, Unit unit2, int power2)
  {
    this (unit1, 1, unit2, power2);
  }

  public String toString ()
  {
    StringBuffer str = new StringBuffer(60);
    String str1 = unit1.toString();
    str.append (str1);
    char last = str1.charAt(str1.length()-1);
    if (! Character.isDigit(last))
      str.append(power1);
    else if (power1 != 1)
      {
	// Kludge:
	str.append("**");
	str.append(power1);
      }
    if (power2 != 0)
      {
	str.append(unit2);
	if (power2 != 1)
	  str.append(power2);
      }
    return str.toString();
  }
}
