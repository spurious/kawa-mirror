package gnu.jemacs.buffer;
import gnu.mapping.*;

/**
 * A Constraint on a Symbol that implements a buffer-local variable.
 */

public class BufferLocal extends IndirectableLocation
{
  boolean all;

  Buffer cachedBuffer;

  /** Index in <code>cachedBuffer</code>'s <code>LocalBindings</code> array. */
  int cachedIndex;

  BufferLocal (Symbol name, boolean all)
  {
    super(name, null);
    this.all = all;
  }

  public static void make(Symbol symbol, boolean all)
  {
    Environment env = Environment.getCurrent();
    Location base = env.getLocation(symbol, null).getBase();
    if (base instanceof BufferLocal)
      {
	if (all)
	  ((BufferLocal) base).all = true;
	return;
      }
    BufferLocal bloc = new BufferLocal(symbol, all);
    env.addLocation(bloc);
  }

  public Object get (Object defaultValue)
  {
    return get(Buffer.getCurrent(), defaultValue);
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
    return isBound(Buffer.getCurrent());
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
    set(Buffer.getCurrent(), newValue);
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
	localBindings[avail+1] = newValue;
      }
    else if (base == null)
      value = newValue;
    else
      base.set(newValue);
  }
}
