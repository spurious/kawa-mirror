 package gnu.mapping;
import gnu.math.*;
import gnu.lists.*;
import java.util.Hashtable;

/** A procedure activation stack (when compiled with explicit stacks). */

public class CallContext implements Runnable
    // extends ValueStack ??? FIXME
{
  // static ThreadLocal currentContext = new ThreadLocal();
  static Hashtable threadMap = new Hashtable(50);
  Thread currentThread;

  /** A marker used to indicate a temporary CallContext.
   * This is so we can create and bind a a CallContext on a fluid-let,
   * and then unbind it from teh current then when the fluid-let finishes.
   * Otherwise, the threadMap might prevent GC of the thread.
   * We could use ThreadLocal variables, though that would require JDK 1.2.
   */
  private static FluidBinding mustRemoveMarker
  = new FluidBinding(null, null, null);

  public CallContext ()
  {
  }

  public CallContext (Thread thread)
  {
    setInstance(thread, this);
  }

  public final Environment getEnvironment()
  {
    if (currentThread instanceof Future)
      return ((Future) currentThread).environment;
    return Environment.global;
  }

  public static CallContext setMainContext()
  {
    Thread thread = Thread.currentThread();
    CallContext ctx = new CallContext(thread);
    defaultContext = ctx;
    return ctx;
  }

  static CallContext defaultContext = null;

  static synchronized void getDefaultContext ()
  {
    if (defaultContext == null)
      defaultContext = new CallContext();
  }

  /** Get or create a CallContext for the current thread. */
  public static CallContext getInstance()
  {
    /*
    CallContext ctx = currentContext.get();
    if (ctx != null)
      {
	ctx = new CallContext(Thread.currentThread());
	currentContext.set(ctx);
      }
    return ctx;
    */

    Thread thread = Thread.currentThread();
    CallContext ctx = getInstance(thread);
    if (ctx == null)
      {
	ctx = new CallContext(thread);
	ctx.fluidBindings = mustRemoveMarker;
      }
    return ctx;
  }

  /** Get the CallContext associated with the current thread.
      Return null if no CallContext has been associated with this thread. */
  public static final CallContext getInstance(Thread thread)
  {
    if (thread instanceof Future)
      return ((Future) thread).getCallContext();
    Object ctx = threadMap.get(thread);
    if (ctx != null)
      return (CallContext) ctx;
    if (defaultContext == null)
      getDefaultContext();
    return defaultContext;
  }

  /** Set the CallContext associated with the current thread. */
  public static final void setInstance(Thread thread, CallContext ctx)
  {
    if (thread instanceof Future)
      ((Future) thread).setCallContext(ctx);
    else if (ctx == null)
      threadMap.remove(thread);
    else
      {
	threadMap.put(thread, ctx);
	ctx.currentThread = thread;
      }
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

  public Object getArgAsObject(int i)
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
	int i = count;
	Object[] args = new Object[i];
	while (--i >= 0)
	  args[i] = getArgAsObject(i);
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
	Binding binding = fluid.binding;
	Constraint constraint = binding.constraint;
	if (constraint instanceof FluidConstraint)
	  ((FluidConstraint) constraint).referenceCount++;
	else
	  binding.constraint = new FluidConstraint(constraint);
      }
    fluidBindings = new_fluids;
  }

  public void resetFluids (FluidBinding old_fluids)
  {
    FluidBinding new_fluids = fluidBindings;
    FluidBinding fluid = new_fluids;
    for ( ; fluid != old_fluids;  fluid = fluid.previous)
      {
	Binding binding = fluid.binding;
	FluidConstraint constraint = (FluidConstraint) binding.constraint;
	if (constraint.referenceCount-- <= 0)
	  binding.constraint = constraint.savedConstraint;
      }
    fluidBindings = old_fluids;

    if (old_fluids == mustRemoveMarker)
      {
	setInstance(currentThread, null);
	currentThread = null;
      }
  }
}
