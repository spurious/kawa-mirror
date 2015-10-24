// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Procedure that evaluates to the value of a Location.
 * Calling it with one argument sets the value, for compatibility
 * with the "parameter objects" of SRFI-39.
 */
public class LocationProc<T> extends Procedure0or1 implements HasSetter
{
  Location<T> loc;

  public LocationProc (Location loc)
  {
    this.loc = loc;
  }

  public static LocationProc makeNamed (Symbol name, Location loc)
  {
    LocationProc lproc = new LocationProc(loc);
    lproc.setSymbol(name);
    return lproc;
  }

  public LocationProc (Location loc, Procedure converter)
  {
    this.loc = loc;
    if (converter != null)
      pushConverter(converter);
  }

  public void pushConverter (Procedure converter)
  {
    loc = ConstrainedLocation.make(loc, converter);
  }

    public final T getValue() throws Throwable {
        return loc.get();
    }

  public T apply0 () throws Throwable
  {
    return loc.get();
  }

  public Object apply1 (Object value) throws Throwable
  {
    set0((T) value);
    return Values.empty;
  }

  public void set0 (Object value) throws Throwable
  {
    loc.set((T) value);
  }

  public Procedure getSetter()
  {
    return new Setter0(this);
  }

  public final Location getLocation ()
  {
    return loc;
  }

  public String toString ()
  {
    Object n = getSymbol();
    if (n != null)
      return super.toString();
    return "#<location-proc "+loc+">";
  }
}
