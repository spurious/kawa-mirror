package kawa.lang;

/** A Location is an abstract cell/location/variable with a value. */

public class Location
{
  /** The current value of the binding. */
  Object value;

  public final Object get ()
  {
    return value;
  }

  public final void set (Object value)
  {
    this.value = value;
  }
}
