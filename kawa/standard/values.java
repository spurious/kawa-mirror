package kawa.standard;
import kawa.lang.*;

public class values extends ProcedureN
{
  public values()
  {
    super("values");
  }

  public Object applyN (Object[] args)
  {
    if (args.length == 1)
      return args[0];
    else
      return new Values (args);
  }
}
