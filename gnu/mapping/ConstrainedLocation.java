// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class ConstrainedLocation extends Location
{
  protected Location base;

  protected Procedure converter;

  public static ConstrainedLocation make (Location base,
					  Procedure converter)
  {
    ConstrainedLocation cloc = new ConstrainedLocation();
    cloc.base = base;
    cloc.converter = converter;
    return cloc;
  }

  public Symbol getKeySymbol ()
  {
    return base.getKeySymbol();
  }

  public Object getKeyProperty ()
  {
    return base.getKeyProperty();
  }

  public boolean isConstant ()
  {
    return base.isConstant();
  }

  public final Object get (Object defaultValue)
  {
    return base.get(defaultValue);
  }
  
  public boolean isBound ()
  {
    return base.isBound();
  }

  protected Object coerce (Object newValue)
  {
    try
      {
	return converter.apply1(newValue);
      }
    catch (Throwable ex)
      {
	throw WrappedException.wrapIfNeeded(ex);
      }
  }

  public final void set (Object newValue)
  {
    base.set(coerce(newValue));
  }

  public Object setWithSave (Object newValue)
  {
    return base.setWithSave(coerce(newValue));
  }

  public void setRestore (Object oldValue)
  {
    base.setRestore(oldValue);
  }
}

