package gnu.mapping;

/**
 * The abstract parent for all Scheme functions.
 * @author  Per Bothner
 */

public abstract class Procedure implements Named
{
  /** If non-null, a sequence of (key, value)-pairs. */
  private Object[] properties;

  private static final String nameKey = "name";
  private static final String setterKey = "setter";

  public String getName()
  {
    return (String) getProperty(nameKey, null);
  }

  /** @deprecated */
  public final String name()
  {
    return getName();
  }

  public final void setName (String name)
  {
    setProperty(nameKey, name);
  }

  public Procedure()
  {
  }

  public Procedure(String n)
  {
    setName(n);
  }

  public abstract Object applyN (Object[] args) throws Throwable;

  public abstract Object apply0 () throws Throwable;

  public abstract Object apply1 (Object arg1) throws Throwable;

  public abstract Object apply2 (Object arg1,Object arg2) throws Throwable;

  public abstract Object apply3 (Object arg1, Object arg2, Object arg3) throws Throwable;

  public abstract Object apply4(Object arg1,Object arg2,
				Object arg3,Object arg4) throws Throwable;

  /** Minimum number of arguments required. */
  public final int minArgs() { return numArgs() & 0xFFF; }

  /** Maximum number of arguments allowed, or -1 for unlimited. */
  public final int maxArgs() { return numArgs() >> 12; }

  /** Check that the number of arguments in a call is valid.
    * @param proc the Procedure being called
    * @param argCount the number of arguments in the call
    * @return nothing, if the number of arguments is ok
    * @exception WrongArguments there are too many or too
    *     few actual arguments
    */
  public static void checkArgCount(Procedure proc, int argCount)
  {
    int num = proc.numArgs();
    if (argCount < (num & 0xFFF)
	|| (num >= 0 && argCount > (num >> 12)))
      throw new WrongArguments(proc, argCount);
  }

  /** Return minArgs()|(maxArgs<<12). */

  /* We use a single virtual function to reduce the number of methods
   * in the system, as well as the number of virtual method table entries.
   * We shift by 12 so the number can normally be represented using a
   * sipush instruction, without requiring a constant pool entry.
   */
  public int numArgs() { return 0xfffff000; }

  /* CPS: ??
  public void apply1(Object arg, CallContext stack, CallFrame rlink, int rpc)
  {
    context.value = apply1(arg);
    context.frame = rlink;
    context.pc = rpc;
  }
  */

  /** Call this Procedure using the explicit-CallContext-convention.
   * The input arguments are (by default) in stack.args;
   * the result is (by default) left in stack.value. */

  public void apply (CallContext ctx) throws Throwable
  {
    Object result;
    int count = ctx.count;
    if (ctx.where == 0 && count != 0)
      result = applyN(ctx.values);
    else
      {
	switch (count)
	  {
	  case 0:
	    result = apply0();
	    break;
	  case 1:
	    result = apply1(ctx.getNextArg());
	    break;
	  case 2:
	    result = apply2(ctx.getNextArg(), ctx.getNextArg());
	    break;
	  case 3:
	    result = apply3(ctx.getNextArg(), ctx.getNextArg(),
			    ctx.getNextArg());
	    break;
	  case 4:
	    result = apply4(ctx.getNextArg(), ctx.getNextArg(),
			    ctx.getNextArg(), ctx.getNextArg());
	    break;
	  default:
	    result = applyN(ctx.getArgs());
	    break;
	  }
      }
    ctx.writeValue(result);
  }

  public Procedure getSetter()
  {
    if (! (this instanceof HasSetter))
      {
	Object setter = getProperty(setterKey, null);
	if (setter instanceof Procedure)
	  return (Procedure) setter;
	throw new RuntimeException("procedure '"+getName()+ "' has no setter");
      }
    int num_args = numArgs();
    if (num_args == 0x0000)
      return new Setter0(this);
    if (num_args == 0x1001)
      return new Setter1(this);
    return new Setter(this);
  }

  public void setSetter (Procedure setter)
  {
    if (this instanceof HasSetter)
      throw new RuntimeException("procedure '"+getName()+
				 "' has builtin setter - cannot be modified");
    setProperty(Procedure.setterKey, setter);
  }

  /** If HasSetter, the Procedure is called in the LHS of an assignment. */
  public void set0(Object result) throws Throwable
  {
    getSetter().apply1(result);
  }

  public void set1(Object arg1, Object value) throws Throwable
  {
    getSetter().apply2(arg1, value);
  }

  public void setN (Object[] args) throws Throwable
  {
    getSetter().applyN(args);
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append ("#<procedure ");
    String n = getName();
    if (n == null)
      n = getClass().getName();
    sbuf.append(n);
    sbuf.append('>');
    return sbuf.toString();
  }

  public Object getProperty(Object key, Object defaultValue)
  {
    if (properties != null)
      {
	for (int i = properties.length;  (i -= 2) >= 0; )
	  {
	    if (properties[i] == key)
	      return properties[i + 1];
	  }
      }
    return defaultValue;
  }

  public synchronized void setProperty(Object key, Object value)
  {
    properties = setProperty(properties, key, value);
  }

  /** Given a property list, update it.
   * @param properties the input property list
   * @param key
   * @param value associate this with key in result
   * @return updated property list (maybe the same as the input)
   */
  public static Object[] setProperty(Object[] properties,
				     Object key, Object value)
  {
    int avail;
    Object[] props = properties;
    if (props == null)
      {
	properties = props = new Object[10];
	avail = 0;
      }
    else
      {
	avail = -1;
	for (int i = props.length;  (i -= 2) >= 0; )
	  {
	    Object k = props[i];
	    if (k == key)
	      {
		Object old = props[i + 1];
		props[i + 1] = value;
		return properties;
	      }
	    else if (k == null)
	      avail = i;
	  }
	if (avail < 0)
	  {
	    avail = props.length;
	    properties = new Object[2 * avail];
	    System.arraycopy(props, 0, properties, 0, avail);
	    props = properties;
	  }
      }
    props[avail] = key;
    props[avail+1] = value;
    return properties;
  }

  public Object removeProperty(Object key)
  {
    Object[] props = properties;
    if (props == null)
      return null;
    for (int i = props.length;  (i -= 2) >= 0; )
      {
	Object k = props[i];
	if (k == key)
	  {
	    Object old = props[i + 1];
	    props[i] = null;
	    props[i + 1] = null;
	    return old;
	  }
      }
    return null;
  }
}
