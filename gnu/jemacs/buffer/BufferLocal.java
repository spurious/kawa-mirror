package gnu.jemacs.buffer;
import gnu.mapping.*;

/**
 * A buffer-local variable (Location).
 */

public class BufferLocal extends IndirectableLocation
{
  boolean all;

  final Symbol name;

  Buffer cachedBuffer;

  /** Index in <code>cachedBuffer</code>'s <code>localBindings</code> array. */
  int cachedIndex;

  BufferLocal (Symbol name, boolean all)
  {
    this.name = name;
    this.all = all;
  }

  public final Symbol getKeySymbol ()
  {
    return name;
  }

  public static void make(Symbol symbol, boolean all)
  {
    Environment env = Environment.getCurrent();
    NamedLocation loc = env.getLocation(symbol, null, true);
    Location base = loc.getBase();
    if (base instanceof BufferLocal)
      {
	if (all)
	  ((BufferLocal) base).all = true;
	return;
      }
    BufferLocal bloc = new BufferLocal(symbol, all);
    // Not sure if this is 100% correct.  FIXME.
    // We have to be careful to avoid cycles, handle INDIERCT_DEFINES, etc.
    bloc.base = loc.getBaseForce();
    bloc.setAlias(loc);
  }

  public Object get (Object defaultValue)
  {
    Buffer buffer = Buffer.getCurrent();
    return buffer == null ? base.get(defaultValue) : get(buffer, defaultValue);
  }

  public Object get (Buffer buffer, Object defaultValue)
  {
    Object[] localBindings = buffer.localBindings;
    if (buffer == cachedBuffer)
      {
	int i = cachedIndex;
	if (i > 0)
	  return localBindings[i];
      }
    else if (localBindings != null)
      {
	Symbol n = this.getKeySymbol();
	int len = localBindings.length;
	cachedBuffer = buffer;
	for (int i = 0;  i < len;  i += 2)
	  {
	    if (localBindings[i] == n)
	      {
		cachedIndex = ++i;
		return localBindings[i];
	      }
	  }
	cachedIndex = 0;
      }
    return base != null ? base.get(defaultValue)
      : value == Location.UNBOUND ? defaultValue : value;
  }

  public boolean isBound ()
  {
    Buffer buffer = Buffer.getCurrent();
    return buffer == null ? base.isBound() : isBound(buffer);
  }

  public boolean isBound (Buffer buffer)
  {
    Object[] localBindings = buffer.localBindings;
    Object unb = Location.UNBOUND;
    if (buffer == cachedBuffer)
      {
	int i = cachedIndex;
	if (i >= 0)
	  return localBindings[i] != unb;
      }
    else if (localBindings != null)
      {
	Symbol n = this.getKeySymbol();
	int len = localBindings.length;
	cachedBuffer = buffer;
	for (int i = 0;  i < len;  i += 2)
	  {
	    if (localBindings[i] == n)
	      {
		cachedIndex = ++i;
		return localBindings[i] != unb;
	      }
	  }
	cachedIndex = 0;
      }
    return super.isBound();
  }

  public synchronized final void set (Object newValue)
  {
    Buffer buffer = Buffer.getCurrent();
    if (buffer == null)
      base.set(newValue);
    else
      set(buffer, newValue);
  }

  public synchronized final void set (Buffer buffer, Object newValue)
  {
    Object[] localBindings = buffer.localBindings;
    int avail = -1;
    Symbol n = this.getKeySymbol();
    if (buffer == cachedBuffer)
      {
	int i = cachedIndex;
	if (i >= 0)
	  {
	    localBindings[i] = newValue;
	    return;
	  }
      }
    else if (localBindings != null)
      {
	int len = localBindings.length;
	for (int i = 0;  i < len;  i += 2)
	  {
	    Object key = localBindings[i];
	    if (key == n)
	      {
		cachedBuffer = buffer;
		cachedIndex = ++i;
		localBindings[i] = newValue;
		return;
	      }
	    if (key == null)
	      avail = i;
	  }
	cachedIndex = 0;
      }
    if (all)
      {
	if (avail < 0)
	  {
	    if (localBindings == null)
	      {
		localBindings = new Object[20];
		buffer.localBindings = localBindings;
		avail = 0;
	      }
	    else
	      {
		avail = localBindings.length;
		Object[] newBindings = new Object[2 * avail];
		System.arraycopy(localBindings, 0, newBindings, 0, avail);
		buffer.localBindings = localBindings = newBindings;
	      }
	  }
	localBindings[avail] = n;
	cachedBuffer = buffer;
	cachedIndex = ++avail;
	localBindings[avail] = newValue;
      }
    else if (base == null)
      value = newValue;
    else
      base.set(newValue);
  }
}
