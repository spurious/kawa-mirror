package kawa.standard;
import gnu.mapping.*;

public class makeProcLocation extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    Procedure proc = (Procedure) args[0];
    int nargs = args.length - 1;
    Object[] xargs = new Object[nargs];
    System.arraycopy(args, 1, xargs, 0, nargs);
    return new ProcLocation(proc, xargs);
  }
}
