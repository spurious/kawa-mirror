package gnu.kawa.reflect;
import gnu.mapping.*;

public class Invoke extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    int len = args.length;
    Procedure.checkArgCount(this, len);
    len -= 2;
    Procedure proc = ClassMethods.apply(this, args[0].getClass(), args[1],
                                        null, null, 0, 0);
    Object[] rargs = new Object[len+1];
    rargs[0] = args[0];
    System.arraycopy(args, 2, rargs, 1, len);
    return proc.applyN(rargs);
  }
}
