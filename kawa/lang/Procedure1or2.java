package kawa.lang;

/**
 * Abstract class for 1- or 2-argument Scheme procedures.
 * Extensions must provide apply1 and apply2.
 * @author	Per Bothner
 */

public abstract class Procedure1or2 extends Procedure
{

  public Procedure1or2 (String n)
  {
    super(n);
  }

  public Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name(), 1,"(? arg1 [arg2])");
  }

  public abstract Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public abstract Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name(), 1,"(? arg1 [arg2])");
  }

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name(), 1,"(? arg1 [arg2])");
  }

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length == 1)
      return apply1 (args[0]);
    else if (args.length == 2)
      return apply2 (args[0], args[1]);
    else
      throw new WrongArguments(this.name(), 1,"(? arg1 [arg2])");
  }
}
