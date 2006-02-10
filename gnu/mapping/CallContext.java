// Copyright (C) 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.math.*;
import gnu.lists.*;

/** A procedure activation stack (when compiled with explicit stacks). */

public class CallContext // implements Runnable
    // extends ValueStack ??? FIXME
{
  /* #ifdef JAVA2 */
  static ThreadLocal currentContext = new ThreadLocal();
  /* #else */
  // static java.util.Hashtable threadMap = new java.util.Hashtable(50);
  /* #endif */
  Thread currentThread;

  Environment curEnvironment;

  public final Environment getEnvironmentRaw() { return curEnvironment; }
  public final void setEnvironmentRaw(Environment env) { curEnvironment = env;}

  public final Environment getEnvironment()
  {
    if (curEnvironment == null)
      {
	Environment env
          = Environment.make(currentThread.getName(), Environment.global);
        env.flags |= Environment.THREAD_SAFE;
        curEnvironment = env;
      }
    return curEnvironment;
  }

  public static void setInstance(CallContext ctx)
  {
    Thread thread = Thread.currentThread();
    ctx.currentThread = thread; 
    /* #ifdef JAVA2 */
    currentContext.set(ctx);
    /* #else */
    // if (thread instanceof Future)
    //   ((Future) thread).closure.context = ctx;
    // else
    //   threadMap.put(thread, ctx);
    /* #endif */
  }

  /** Get but don't create a CallContext for the current thread. */
  public static CallContext getOnlyInstance()
  {
    /* #ifdef JAVA2 */
    return (CallContext) currentContext.get();
    /* #else */
    // Thread thread = Thread.currentThread();
    // if (thread instanceof Future)
    //   return ((Future) thread).getCallContext();
    // return (CallContext) threadMap.get(thread);
    /* #endif */
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

  /** The program location in the current procedure.
   * This a selector that only has meaning to the proc's Procedure.*/
  public int pc;

  /* CPS:
  CallFrame frame;
  */

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
  public int where;
  public final static int ARG_IN_VALUES_ARRAY = 0;
  public final static int ARG_IN_VALUE1 = 1;
  public final static int ARG_IN_VALUE2 = 2;
  public final static int ARG_IN_VALUE3 = 3;
  public final static int ARG_IN_VALUE4 = 4;
  public final static int ARG_IN_IVALUE1 = 5;
  public final static int ARG_IN_IVALUE2 = 6;

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
   * FIXME: This and following methods don't really fit until the
   * current match/apply-based API, at least as currently implemented.
   * We probably need to pass in (or make this a method of) the Procedure.
   */
  public Object getNextArg()
  {
    if (next >= count)
      throw new WrongArguments(null, count);
    return getArgAsObject(next++);
  }

  public int getNextIntArg()
  {
    if (next >= count)
      throw new WrongArguments(null, count);
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
  public final Object[] getRestArgsArray (int next)
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
  public final LList getRestArgsList (int next)
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
      throw new WrongArguments(null, count);
    values = null;
  }

  public Object[] getArgs()
  {
    if (where == 0)
      return values;
    else
      {
	int n = count;
	next = 0;
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
	Procedure proc = this.proc;
	if (proc == null)
	  {
	    /* CPS:
	    CallFrame fr = frame;
	    if (fr == null)
	      break;
	    proc = fr.proc;
	    frame = fr.previous;
	    if (proc == null)
	    */
	      break;
	  }
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

  /** Write values (of function result) to current consumer. */
  public void writeValue(Object value)
  {
    Values.writeValues(value, consumer);
  }

  protected String baseUri;
  protected static String baseUriDefault;

  public static String getBaseUriDefault ()
  {
    String uri = baseUriDefault;
    if (uri == null)
      {
        /* #ifdef use:java.net.URI */
        uri = new java.io.File("").toURI().toString();
        /* #else */
	// uri = System.getProperty("user.dir");	
	// if (uri == null)
        //   return null;
	// char sep = java.io.File.separatorChar;
        // /* Note this does not encode illegal characters using '%'. */
	// if (sep != '/')
	//   uri = uri.replace(sep, '/');
	// uri = ((uri.charAt(0) == '/' ? "file:" : "file:/")
	// 	   + uri + '/');
        /* #endif */
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

  /** A stack of currently re-bound fluids variables.
   * There is one for each active fluids-let or parmaterize variable. */
  Location[] pushedFluids;
  /** The number of active elements of the pushedFluids array. */
  int pushedFluidsCount;

  public final void pushFluid (Location loc)
  {
    Location[] fluids = pushedFluids;
    int count = pushedFluidsCount;
    if (fluids == null)
      {
        pushedFluids = fluids = new Location[10];
      }
    else if (count == fluids.length)
      {
        Location[] newFluids = new Location[2 * count];
        System.arraycopy(fluids, 0, newFluids, 0, count);
        pushedFluids = fluids = newFluids;
      }
    fluids[count] = loc;
    pushedFluidsCount = count + 1;
  }

  public final void popFluid ()
  {
    pushedFluids[--pushedFluidsCount] = null;
  }

  /** Current stack of evaluation frames for interpreter. */
  public Object[][] evalFrames;
}

/* CPS:
class CallFrame
{
  Procedure proc;
  CallFrame previous;
  int saveVstackLen;

  // Should probably be in sub-classes of ClassFrame:
  Object[] values;
}
*/
