package gnu.mapping;

public class RunnableClosure
  implements
  /* #ifdef JAVA5 */
  java.util.concurrent.Callable<Object>,
  /* #endif */
  Runnable
{
  Object result;
  CallContext context;
  public Environment environment;

  // These are only used to when we need to override the parents' in/out/err
  // in the child.  This is not needed for normal RunnableClosure objects, but (say)
  // when starting a repl in a new window.  In that case we could do te
  // in/out/err override in the 'action'.  FIXME.
  private InPort in;
  private OutPort out;
  private OutPort err;
  Throwable exception;

  Procedure action;
  String name;

  static int nrunnables=0;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name=name;
  }

  public RunnableClosure (Procedure action, CallContext parentContext)
  {
    this(action, parentContext, parentContext.getEnvironment());
  }

  public RunnableClosure (Procedure action,
			  CallContext parentContext, Environment penvironment)
  {
    setName("r"+nrunnables++);
    this.action = action;
    SimpleEnvironment env = Environment.make(getName(), penvironment); 
    env.flags |= Environment.THREAD_SAFE;
    env.flags &= ~Environment.DIRECT_INHERITED_ON_SET;
    this.environment = env;
    int n = parentContext.pushedFluidsCount;
    for (int i = 0;  i < n; i++)
      {
        // If we're inside a fluid-let, then the child thread should inherit
        // the fluid-let binding, even if it isn't accessed in the child until
        // after the parent exits the fluid-let.  Set things up so only the
        // binding in the parent is restored, but they share until then.
        Location loc = parentContext.pushedFluids[i];
        Symbol name = loc.getKeySymbol();
        Object property = loc.getKeyProperty();
        if (name != null && loc instanceof NamedLocation)
          {
            NamedLocation nloc = (NamedLocation) loc;
            if (nloc.base == null)
              {
                SharedLocation sloc = new SharedLocation(name, property, 0);
                sloc.value = nloc.value;
                nloc.base = sloc;
                nloc.value = null;
                nloc = sloc;
              }
            int hash = name.hashCode() ^ System.identityHashCode(property);
            NamedLocation xloc = env.addUnboundLocation(name, property, hash);
            xloc.base = nloc;
          }
      }
  }

  public RunnableClosure (Procedure action, Environment penvironment,
			  InPort in, OutPort out, OutPort err)
  {
    this(action, CallContext.getInstance(), penvironment);
    this.in = in;
    this.out = out;
    this.err = err;
  }

  public RunnableClosure (Procedure action)
  {
    this(action, CallContext.getInstance());
  }

  /** Get the CallContext we use for this Thread. */
  public final CallContext getCallContext() { return context; }

  public void run ()
  {
    try
      {
        if (context == null)
          context = CallContext.getInstance();
        else
          CallContext.setInstance(context);
	context.curEnvironment = environment;
	if (in != null)
	  InPort.setInDefault(in);
	if (out != null)
	  OutPort.setOutDefault(out);
	if (err != null)
	  OutPort.setErrDefault(err);
	result = action.apply0 ();
      }
    catch (Throwable ex)
      {
	exception = ex;
      }
  }

  /** Get the result of running this {@code Runnable}.
   * The result is a value or a thrown exception.
   * Should be called after {#code run} finishes. */
  Object getResult () throws Throwable
  {
    Throwable ex = exception;
    if (ex != null)
      throw ex;
    return result;
  }

  public Object call()
    throws Exception
  {
    run();
    Throwable ex = exception;
    if (ex != null)
      {
        if (ex instanceof Exception)
          throw (Exception) ex;
        else if (ex instanceof Error)
          throw (Error) ex;
        else
          throw new RuntimeException(ex);
      }
    return result;
  }

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append ("#<runnable ");
    buf.append(getName());
    buf.append(">");
    return buf.toString();
  }
}
