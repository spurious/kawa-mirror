package kawa.lang;

/**
 * Abstract class for "N-argument" Scheme procedures, where N>4 or variable.
 * @author	Per Bothner
 */

public abstract class ProcedureN extends Procedure
{
  public ProcedureN ()
  {
    super();
  }

  public ProcedureN(java.lang.String n)
  {
      super(n);
  }

  public Object apply0 ()
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return applyN (new Object[0]);
  }

  public Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return applyN (args);
  }

   public Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return applyN (args);
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return applyN (args);
  }

  public Object apply4 (Object arg1, Object arg2,
			Object arg3, Object arg4) 
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return applyN (args);
  }

  public abstract Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;
}
