// Copyright (C) 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.math.*;
import gnu.lists.*;
import java.util.Hashtable;

/** A procedure activation stack (when compiled with explicit stacks). */

public class CallContext implements Runnable
    // extends ValueStack ??? FIXME
{
  /* BEGIN JAVA2 */
  static ThreadLocal currentContext = new ThreadLocal();
  /* END JAVA2 */
  /* BEGIN JAVA1 */
  // static Hashtable threadMap = new Hashtable(50);
  /* END JAVA1 */
  Thread currentThread;

  Environment curEnvironment;

  public final Environment getEnvironment()
  {
    return curEnvironment != null ? curEnvironment : Environment.global;
  }

  public static void setInstance(CallContext ctx)
  {
    Thread thread = Thread.currentThread();
    ctx.currentThread = thread; 
    /* BEGIN JAVA2 */
    currentContext.set(ctx);
    /* END JAVA2 */
    /* BEGIN JAVA1 */
    // if (thread instanceof Future)
    //   ((Future) thread).context = ctx;
    // else
    //   threadMap.put(thread, ctx);
    /* END JAVA1 */
  }

  /** Get but don't create a CallContext for the current thread. */
  public static CallContext getOnlyInstance()
  {
    /* BEGIN JAVA2 */
    return (CallContext) currentContext.get();
    /* END JAVA2 */
    /* BEGIN JAVA1 */
    // Thread thread = Thread.currentThread();
    // if (thread instanceof Future)
    //   return ((Future) thread).getCallContext();
    // return (CallContext) threadMap.get(thread);
    /* END JAVA1 */
  }

  /** Get or create a CallContext for the current thread. */
  public static CallContext getInstance()
  {
    CallContext ctx = getOnlyInstance();
    if (ctx == null)
      {
	ctx = new CallContext();
	setInstance(ctx);
      }
    return ctx;
  }

  public Procedure proc;

  /* CPS: ??
  CallFrame frame;
  */

  /** The program location in the current procedure. */
  public int pc;

  /** Default place for function results.
   * In the future, function arguments will also use vstack. */
  public ValueStack vstack = new ValueStack();  // ?? super
  /** Function results are written to this Consumer.
   * This may point to vstack - or some other Consumer. */
  public Consumer consumer = vstack;

  /** Used for passing parameters.  (Will be replaced by vstack.) */
  public Object value1;
  public Object value2;
  public Object value3;
  public Object value4;
  public Object[] values;
  public int ivalue1;
  public int ivalue2;

  /** Number of actual arguments. */
  public int count;
  
  /** Index of next argument.
   * This is used by methods like getNextArg, used by callees. */
  public int next;

  /** Encoding of where the arguments are.
   * Each argument uses 4 bits.
   * Arguments beyond 8 are implicitly ARG_IN_VALUES_ARRAY.
   */
  int where;
  final static int ARG_IN_VALUES_ARRAY = 0;
  final static int ARG_IN_VALUE1 = 1;
  final static int ARG_IN_VALUE2 = 2;
  final static int ARG_IN_VALUE3 = 3;
  final static int ARG_IN_VALUE4 = 4;
  final static int ARG_IN_IVALUE1 = 5;
  final static int ARG_IN_IVALUE2 = 6;

  Object getArgAsObject(int i)
  {
    if (i < 8)
      {
        switch ((this.where >> (4 * i)) & 15)
          {
          case ARG_IN_VALUE1:  return value1;
          case ARG_IN_VALUE2:  return value2;
          case ARG_IN_VALUE3:  return value3;
          case ARG_IN_VALUE4:  return value4;
          case ARG_IN_IVALUE1:  return IntNum.make(ivalue1);
          case ARG_IN_IVALUE2:  return IntNum.make(ivalue2);
          }
      }
    return values[i];
  }

  /** Get the next incoming argument.
   * Throw WrongArguments if there are no more arguments.
   */
  public Object getNextArg()
  {
    if (next >= count)
      throw new WrongArguments(proc, count);
    return getArgAsObject(next++);
  }

  public int getNextIntArg()
  {
    if (next >= count)
      throw new WrongArguments(proc, count);
    Object arg = getArgAsObject(next++);
    return ((Number) arg).intValue();
  }

  /** Get the next incoming argument.
   * Return defaultValue if there are no more arguments.
   */
  public Object getNextArg(Object defaultValue)
  {
    if (next >= count)
      return defaultValue;
    return getArgAsObject(next++);
  }

  public int getNextIntArg(int defaultValue)
  {
    if (next >= count)
      return defaultValue;
    return ((Number) getArgAsObject(next++)).intValue();
  }

  /** Get remaining arguments as an array. */
  public final Object[] getRestArgsArray ()
  {
    Object[] args = new Object[count - next];
    int i = 0;
    while (next < count)
      {
	args[i++] = getArgAsObject(next++);
      }
    return args;
  }

  /** Get remaining arguments as a list.
   * Used for Scheme and Lisp rest args. */
  public final LList getRestArgsList ()
  {
    LList nil = LList.Empty;
    LList list = nil;
    Pair last = null;
    while (next < count)
      {
	Pair pair = new Pair(getArgAsObject(next++), nil);
	if (last == null)
	  list = pair;
	else
	  last.cdr = pair;
	last = pair;
      }
    return list;
  }

  /** Note that we are done with the input arguments.
   * Throw WrongArguments if there are unprocessed arguments.
   */
  public void lastArg()
  {
    if (next < count)
      throw new WrongArguments(proc, count);
    values = null;
  }

  public void setArgs()
  {
    count = 0;
    where = 0;
    next = 0;
  }

  public void setArgs(Object arg1)
  {
    value1 = arg1;
    count = 1;
    where = ARG_IN_VALUE1;
    next = 0;
  }

  public void setArgs(Object arg1, Object arg2)
  {
    value1 = arg1;
    value2 = arg2;
    count = 2;
    where = ARG_IN_VALUE1|(ARG_IN_VALUE2<<4);
    next = 0;
  }
  public void setArgs(Object arg1, Object arg2, Object arg3)
  {
    value1 = arg1;
    value2 = arg2;
    value3 = arg3;
    count = 3;
    where = ARG_IN_VALUE1|(ARG_IN_VALUE2<<4)|(ARG_IN_VALUE3<<8);
    next = 0;
  }

  public void setArgs(Object arg1, Object arg2, Object arg3, Object arg4)
  {
    value1 = arg1;
    value2 = arg2;
    value3 = arg3;
    value4 = arg4;
    count = 4;
    where = (ARG_IN_VALUE1|(ARG_IN_VALUE2<<4)
      |(ARG_IN_VALUE3<<8|ARG_IN_VALUE4<<12));
    next = 0;
  }

  public void setArgsN(Object[] args)
  {
    values = args;
    count = args.length;
    where = 0;
    next = 0;
  }

  public Object[] getArgs()
  {
    if (where == 0)
      return values;
    else
      {
	int n = count;
	Object[] args = new Object[n];
	for (int i = 0;  i < n;  i++)
	  args[i] = getNextArg();
	return args;
      }
  }

  public void runUntilDone()  throws Throwable
  {
    for (;;)
      {
	/** Cps
	CallFrame frame = this.frame;
	if (frame == null)
	  break;
	frame.step(this);
	*/
	Procedure proc = this.proc;
	if (proc == null)
	  break;
	this.proc = null;
	proc.apply(this);
      }
  }

  /** Setup routine before calling a method that takes a CallContext.
   * The compiler emits a call to this before a call to a method that takes
   * a CallContext, when it wants the function result as an Object.
   * It pushes the CallContest state so it can uses the vstack for a
   * temporary, After the method, getFromContext extract the method's result
   * from the vstack and restores the state.
   */
  public final int startFromContext ()
  {
    ValueStack vst = vstack;
    int oindex = vst.find(consumer);
    vst.ensureSpace(3);
    int gapStart = vst.gapStart;
    vst.data[gapStart++] = TreeList.INT_FOLLOWS;
    vst.setIntN(gapStart, oindex);
    gapStart += 2;
    consumer = vst;
    vst.gapStart = gapStart;
    return gapStart;
  }

  /** Routine to extract result and restore state after startFromContext.
   */
  public final Object getFromContext (int oldIndex) throws Throwable
  {
    runUntilDone();
    ValueStack vst = vstack;
    Object result = Values.make(vst, oldIndex, vst.gapStart);
    cleanupFromContext(oldIndex);
    return result;
  }

  /** Cleanup-only part of getFromContext.
   * This can be in an exception handler as an alternative
   * to getFromContext, which is called in the non-exception case.
   * (Alternatively, the compiler could call cleanupFromContext
   * from a finally clause but that is less efficient, partly
   * because the JVM stack must be empty before a finally subroutine.)
   */
  public final void cleanupFromContext (int oldIndex) throws Throwable
  {
    ValueStack vst = vstack;
    char[] data = vst.data;
    int oindex = (data[oldIndex-2] << 16) | (data[oldIndex -1] & 0xFFFF);
    consumer = (Consumer) vst.objects[oindex];
    vst.objects[oindex] = null;
    vst.oindex = oindex;
    vst.gapStart = oldIndex - 3;
  }

  /** Run until no more continuations, returning final result. */
  public final Object runUntilValue() throws Throwable
  {
    Consumer consumerSave = consumer;
    ValueStack vst = vstack;
    consumer = vst;
    int dindexSave = vst.gapStart;
    int oindexSave = vst.oindex;
    try
      {
	runUntilDone();
	return Values.make(vst, dindexSave, vst.gapStart);
      }
    finally
      {
	consumer = consumerSave;
	vst.gapStart = dindexSave;
	vst.oindex = oindexSave;
      }
  }

  /** Run until no more continuations, sending result to a COnsumer. */
  public final void runUntilValue(Consumer out) throws Throwable
  {
    Consumer consumerSave = consumer;
    consumer = out;
    try
      {
	runUntilDone();
      }
    finally
      {
	consumer = consumerSave;
      }
  }

  public void run()
  {
    try
      {
	runUntilDone();
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  /** Write values (of function result) to current consumer. */
  public void writeValue(Object value)
  {
    Values.writeValues(value, consumer);
  }

  public FluidBinding fluidBindings;

  public void setFluids (FluidBinding new_fluids)
  {
    FluidBinding old_fluids = fluidBindings;
    FluidBinding fluid = new_fluids;
    for ( ; fluid != old_fluids;  fluid = fluid.previous)
      {
	Symbol symbol = fluid.symbol;
	synchronized (symbol)
	  {
	    Constraint constraint = symbol.constraint;
	    if (constraint instanceof FluidConstraint)
	      ((FluidConstraint) constraint).referenceCount++;
	    else
	      symbol.constraint = new FluidConstraint(constraint);
	  }
      }
    fluidBindings = new_fluids;
  }

  public void resetFluids (FluidBinding old_fluids)
  {
    FluidBinding new_fluids = fluidBindings;
    FluidBinding fluid = new_fluids;
    for ( ; fluid != old_fluids;  fluid = fluid.previous)
      {
	Symbol symbol = fluid.symbol;
	synchronized (symbol)
	  {
	    FluidConstraint constraint = (FluidConstraint) symbol.constraint;
	    if (constraint.referenceCount-- <= 0)
	      symbol.constraint = constraint.savedConstraint;
	  }
      }
    fluidBindings = old_fluids;
  }

  protected String baseUri;
  protected static String baseUriDefault;

  public static String getBaseUriDefault ()
  {
    String uri = baseUriDefault;
    if (uri == null)
      {
	uri = System.getProperty("user.dir");	
	if (uri == null)
	  return null;
	char sep = java.io.File.separatorChar;
	// Should also encode illegal characters using '%'.  FIXME.
	if (sep != '/')
	  uri = uri.replace(sep, '/');
	uri = ((uri.charAt(0) == '/' ? "file://" : "file:///")
		   + uri + '/');
	baseUriDefault = uri;
      }
    return uri;
  }

  public String getBaseUriRaw ()
  {
    return baseUri;
  }

  /* Get the current "base URI", which defaults to the current directory.
   * However, it may get reset to the "current document". */
  public String getBaseUri ()
  {
    String uri = baseUri;
    if (uri == null)
      baseUri = uri = getBaseUriDefault();
    return uri;
  }

  /** Set the current "base URI". */
  public void setBaseUri (String baseUri)
  {
    this.baseUri = baseUri;
  }
}
