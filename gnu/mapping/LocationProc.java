// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Procedure that evaluates to the value of a Location.
 * Calling it with one argument sets the value, for compatibility
 * with the "parameter objects" of SRFI-39.
 */
public class LocationProc extends Procedure0or1 implements HasSetter
{
  // FIXME Having the converter as part of the LocationProc is ugly.
  // For one thing, it messes up ProcLocation.
  // Instead, the 'loc' should be a "constrained Location".
  // In that case kawa.standard.location.makeProcLocation$V could be
  // optimized to return a ProcLocation of the argument is a LocationProc.
  Procedure converter;
  Location loc;

  public LocationProc (Location loc)
  {
    this.loc = loc;
  }

  public LocationProc (Location loc, Procedure converter)
  {
    this.loc = loc;
    this.converter = converter;
  }

  public Object apply0 () throws Throwable
  {
    return loc.get();
  }

  public Object apply1 (Object value) throws Throwable
  {
    set0(value);
    return Values.empty;
  }

  public void set0 (Object value) throws Throwable
  {
    if (converter != null)
      value = converter.apply1(value);
    loc.set(value);
  }

  public Procedure getSetter()
  {
    return new Setter0(this);
  }

  public final Location getLocation ()
  {
    return loc;
  }

  public Object setWithSave (Object newValue)
  {
    return loc.setWithSave(newValue);
  }

  public void setRestore (Object oldValue)
  {
    loc.setRestore(oldValue);
  }

  public String toString () { return "#<location-proc "+loc+">"; }
}
