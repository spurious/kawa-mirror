package gnu.mapping;
import gnu.lists.*;

public abstract class CpsProcedure extends MethodProc
{
  public CpsProcedure (String n)
  {
    setName(n);
  }

  public CpsProcedure ()
  {
  }

  public abstract void apply (CallContext stack) throws Throwable;

  public Object applyN (Object[] args) throws Throwable
  {
    int count = args.length;
    checkArgCount(this, count);
    Thread thread = Thread.currentThread();
    CallContext ctx = CallContext.getInstance();
    ctx.setArgsN(args);
    ctx.proc = this;
    return applyV(ctx);
  }

  // FIXME - only checks argument length.
  public int match (CallContext ctx, Object[] args)
  {
    int argCount = args.length;
    int num = numArgs();
    int min = num & 0xFFF;
    if (argCount < min)
      return NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
        if (argCount > max)
          return NO_MATCH_TOO_MANY_ARGS|max;
      }
    ctx.setArgsN(args);
    ctx.proc = this;
    return 0;
  }

  public Object applyV(CallContext ctx) throws Throwable
  {
    return ctx.runUntilValue();
  }
}
