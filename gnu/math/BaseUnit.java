// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./LICENSE.

package gnu.math;

/** A primitive Unit of measurement (such as a meter).
 * @author Per Bothner
 */

public class BaseUnit extends Unit
{
  /* BaseUnits are numberd with globally unique indexes. */
  static int base_count = 0;
  int index;

  /** The name of the "dimension" this is a base unit for. */
  String dimension;

  // Only used to create Dimensions.endDummy.
  BaseUnit (int index) { this.index = index; }

  public BaseUnit (String name)
  {
    super ();
    this.name = name;
    this.index = BaseUnit.base_count++;
    this.dims = new Dimensions (this);
    Unit.unitTable.put (name, this);
  }

  public BaseUnit (String name, String dimension)
  {
    this (name);
    this.dimension = dimension;
  }

  public int hashCode () { return index; }
  public Unit unit() { return this; }
}
