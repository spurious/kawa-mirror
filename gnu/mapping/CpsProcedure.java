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

  public abstract void apply (CallStack stack);

  public Object applyN (Object[] args)
  {
    CallStack stack = new CallStack();
    stack.args = args;
    stack.proc = this;
    stack.run();
    return stack.value;
  }

  public Object getVarBuffer()
  {
    return new CallStack();
  }

  // FIXME - only checks argument length.
  public RuntimeException match (Object vars, Object[] args)
  {
    CallStack stack = (CallStack) vars;
    int argCount = args.length;
    int num = numArgs();
    if (argCount < (num & 0xFFF)
	|| (num >= 0 && argCount > (num >> 12)))
      return new WrongArguments(this, argCount);
    stack.args = args;
    stack.proc = this;
    return null;
  }

  public Object applyV(Object vars)
  {
    CallStack stack = (CallStack) vars;
    stack.run();
    return stack.value;
  }
}
