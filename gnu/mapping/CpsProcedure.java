package gnu.mapping;

public abstract class CpsProcedure extends ProcedureN
{
  public CpsProcedure (String n)
  {
    super(n);
  }

  public CpsProcedure ()
  {
    super();
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
}
