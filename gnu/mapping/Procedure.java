// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

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
    Object symbol = getProperty(nameKey, null);
    return symbol == null ? null
      : symbol instanceof Symbol ? ((Symbol) symbol).getName()
      : symbol.toString();
  }

  public Object getSymbol()
  {
    return getProperty(nameKey, null);
  }

  /** @deprecated */
  public final String name()
  {
    return getName();
  }

  public final void setSymbol (Object name)
  {
    setProperty(nameKey, name);
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
   * the result is written to ctx.consumer. */

  public void apply (CallContext ctx) throws Throwable
  {
    apply(this, ctx);
  }

  public static void apply (Procedure proc, CallContext ctx) throws Throwable
  {
    Object result;
    int count = ctx.count;
    if (ctx.where == 0 && count != 0)
      result = proc.applyN(ctx.values);
    else
      {
	switch (count)
	  {
	  case 0:
	    result = proc.apply0();
	    break;
	  case 1:
	    Object a = ctx.getNextArg();
	    result = proc.apply1(a);
	    break;
	  case 2:
	    result = proc.apply2(ctx.getNextArg(), ctx.getNextArg());
	    break;
	  case 3:
	    result = proc.apply3(ctx.getNextArg(), ctx.getNextArg(),
				 ctx.getNextArg());
	    break;
	  case 4:
	    result = proc.apply4(ctx.getNextArg(), ctx.getNextArg(),
				 ctx.getNextArg(), ctx.getNextArg());
	    break;
	  default:
	    result = proc.applyN(ctx.getArgs());
	    break;
	  }
      }
    ctx.writeValue(result);
  }

  /** Pass zero arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match0 (CallContext ctx)
  {
    int num = numArgs();
    int min = num & 0xFFF;
    if (min > 0)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num < 0)
      return matchN(ProcedureN.noArgs, ctx);
    ctx.count = 0;
    ctx.where = 0;
    ctx.next = 0;
    ctx.proc = this;
    return 0;
  }

  /** Pass one argument.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match1 (Object arg1, CallContext ctx)
  {
    int num = numArgs();
    int min = num & 0xFFF;
    if (min > 1)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 1)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.count = 1;
	ctx.where = CallContext.ARG_IN_VALUE1;
	ctx.next = 0;
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1 };
    return matchN(args, ctx);
  }

  /** Pass two arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    int num = numArgs();
    int min = num & 0xFFF;
    if (min > 2)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 2)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.value2 = arg2;
	ctx.count = 2;
	ctx.where = CallContext.ARG_IN_VALUE1
	  |(CallContext.ARG_IN_VALUE2<<4);
	ctx.next = 0;
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1, arg2 };
    return matchN(args, ctx);
  }

  /** Pass three arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    int num = numArgs();
    int min = num & 0xFFF;
    if (min > 3)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 3)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.value2 = arg2;
	ctx.value3 = arg3;
	ctx.count = 3;
	ctx.where = CallContext.ARG_IN_VALUE1
	  |(CallContext.ARG_IN_VALUE2<<4)
	  |(CallContext.ARG_IN_VALUE3<<8);
	ctx.next = 0;
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1, arg2, arg3 };
    return matchN(args, ctx);
  }

  /** Pass four arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    int num = numArgs();
    int min = num & 0xFFF;
    if (min > 4)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 4)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.value2 = arg2;
	ctx.value3 = arg3;
	ctx.value4 = arg4;
	ctx.count = 4;
	ctx.where = (CallContext.ARG_IN_VALUE1
		     |(CallContext.ARG_IN_VALUE2<<4)
		     |(CallContext.ARG_IN_VALUE3<<8)
		     |(CallContext.ARG_IN_VALUE4<<12));
	ctx.next = 0;
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1, arg2, arg3, arg4 };
    return matchN(args, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    int num = numArgs();
    int min = num & 0xFFF;
    if (args.length < min)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
	switch (args.length)
	  {
	  case 0:
	    return match0(ctx);
	  case 1:
	    return match1(args[0], ctx);
	  case 2:
	    return match2(args[0], args[1], ctx);
	  case 3:
	    return match3(args[0], args[1], args[2], ctx);
	  case 4:
	    return match4(args[0], args[1], args[2], args[3], ctx);
	  default:
	    int max = num >> 12;
	    if (args.length > max)
	      return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	  }
      }
    ctx.values = args;
    ctx.count = args.length;
    ctx.where = 0;
    ctx.next = 0;
    ctx.proc = this;
    return 0;
  }

  /** Does match0, plus throws exception on argument mismatch. */
  public void check0 (CallContext ctx)
  {
    int code = match0(ctx);
    if (code != 0)
      {
	throw MethodProc.matchFailAsException(code, this, ProcedureN.noArgs);
      }
  }

  /** Does match1, plus throws exception on argument mismatch. */
  public void check1 (Object arg1, CallContext ctx)
  {
    int code = match1(arg1, ctx);
    if (code != 0)
      {
	Object[] args = { arg1 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }

  /** Does match, plus throws exception on argument mismatch. */
  public void check2 (Object arg1, Object arg2, CallContext ctx)
  {
    int code = match2(arg1, arg2, ctx);
    if (code != 0)
      {
	Object[] args = { arg1, arg2 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }
 
  /** Does match3, plus throws exception on argument mismatch. */
  public void check3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    int code = match3(arg1, arg2, arg3, ctx);
    if (code != 0)
      {
	Object[] args = { arg1, arg2, arg3 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }

  /** Does match4, plus throws exception on argument mismatch. */
  public void check4 (Object arg1, Object arg2, Object arg3, Object arg4,
		      CallContext ctx)
  {
    int code = match4(arg1, arg2, arg3, arg4, ctx);
    if (code != 0)
      {
	Object[] args = { arg1, arg2, arg3, arg4 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }

  /** Does matchN, plus throws exception on argument mismatch. */
  public void checkN (Object[] args, CallContext ctx)
  {
    int code = matchN(args, ctx);
    if (code != 0)
      {
	throw MethodProc.matchFailAsException(code, this, args);
      }
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
