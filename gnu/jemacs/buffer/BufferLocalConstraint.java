package gnu.jemacs.buffer;
import gnu.mapping.*;

/**
 * A Constraint on a Binding that implements buffer-local variable.
 */

public class BufferLocalConstraint extends Constraint
{
  Constraint oldConstraint;

  boolean all;

  Buffer lastBuffer;

  /** Value bound in lastBuffer.
   * We use 'lastValue==this' to indicate there is no local binding. */
  Object lastValue;

  Object[] bufferBindings;
  
  public static void make(Binding binding, boolean all)
  {
    Constraint oldConstraint = getConstraint(binding);
    BufferLocalConstraint newConstraint;
    if (oldConstraint instanceof BufferLocalConstraint)
      newConstraint = (BufferLocalConstraint) oldConstraint;
    else
      {
	newConstraint = new BufferLocalConstraint();
	newConstraint.all = all;
	newConstraint.oldConstraint = oldConstraint;

	binding.setConstraint(newConstraint);
      }
    if (! all)
      {
	Buffer buffer = Buffer.getCurrent();
	if (newConstraint.lastBuffer != buffer)
	  newConstraint.save();
	newConstraint.lastBuffer = buffer;
	newConstraint.lastValue = newConstraint;
      }
  }

  private void save()
  {
    if (lastBuffer == null || lastValue == this)
      return;
    if (bufferBindings == null)
      {
	bufferBindings = new Object[20];
	bufferBindings[0] = lastBuffer;
	bufferBindings[1] = lastValue;
	return;
      }
    int len = bufferBindings.length;
    for (int i = 0;  i < len;  i += 2)
      {
	if (bufferBindings[i] == lastBuffer)
	  {
	    bufferBindings[i+1] = lastValue;
	    return;
	  }
      }
    Object[] newBindings = new Object[2 * len];
    System.arraycopy(bufferBindings, 0, newBindings, 0, len);
    newBindings[len] = lastBuffer;
    newBindings[len+1] = lastValue;
    bufferBindings = newBindings;
    
  }

  public boolean isBound (Binding binding)
  {
    Buffer buffer = Buffer.getCurrent();
    if (buffer == lastBuffer)
      {
	if (lastValue != this)
	  return true;
      }
    else if (bufferBindings != null)
      {
	save();
	lastBuffer = buffer;
	int len = bufferBindings.length;
	for (int i = 0;  i < len;  i += 2)
	  {
	    if (bufferBindings[i] == buffer)
	      {
		lastValue = bufferBindings[i+1];
		if (lastValue != this)
		  return true;
		break;
	      }
	  }
	lastValue = this;
      }
    return oldConstraint.isBound(binding);
  }

  public Object get (Binding binding, Object defaultValue)
  {
    Buffer buffer = Buffer.getCurrent();
    if (buffer == lastBuffer)
      {
	if (lastValue != this)
	  return lastValue;
      }
    else if (bufferBindings != null)
      {
	int len = bufferBindings.length;
	for (int i = 0;  i < len;  i += 2)
	  {
	    if (bufferBindings[i] == buffer)
	      {
		save();
		lastBuffer = buffer;
		lastValue = bufferBindings[i+1];
		if (lastValue != this)
		  return lastValue;
		break;
	      }
	  }
      }
    lastValue = oldConstraint.get(binding, defaultValue);
    lastBuffer = buffer;
    return lastValue;
  }

  public void set (Binding binding, Object value)
  {
    Buffer buffer = Buffer.getCurrent();
    if (buffer != lastBuffer)
      {
	save();
	lastBuffer = buffer;
	int len = bufferBindings.length;
	for (int i = 0;  ;  i += 2)
	  {
	    if (i == len)
	      {
		lastValue = this;
		break;
	      }
	    if (bufferBindings[i] == buffer)
	      {
		lastValue = bufferBindings[i+1];
		break;
	      }
	  }
      }
    if (all || lastValue != this)
      {
	lastValue = value;
	return;
      }
    oldConstraint.set(binding, value);
  }

  public Environment getEnvironment (Binding binding)
  {
    return oldConstraint.getEnvironment(binding);
  }
}
