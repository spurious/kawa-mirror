package gnu.mapping;

public abstract class CpsProcedure extends MethodProc
{
  public CpsProcedure (String n)
  {
    setName(n);
  }

  public CpsProcedure ()
  {
  }

  public abstract void apply (CallContext stack);

  public Object applyN (Object[] args)
  {
    int count = args.length;
    int num = this.numArgs();
    if (count < (num & 0xFFF)
        || (num >= 0 && count > (num >> 12)))
      throw new WrongArguments(this, count);
    CallContext stack = new CallContext();
    stack.values = args;
    stack.proc = this;
    stack.run();
    return stack.value;
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
    ctx.values = args;
    ctx.proc = this;
    return 0;
  }

  public Object applyV(CallContext ctx)
  {
    ctx.run();
    return ctx.value;
  }
}
