package kawa.lang;

/**
 * Abstract class for 1-argument Scheme procedures.
 * @author	Per Bothner
 */

public abstract class Procedure1 extends Procedure
{

  public Procedure1(java.lang.String n)
  {
    super(n);
  }

  public Object apply0 ()
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new kawa.lang.WrongArguments(this.name,1,"(?)");
  }

  public abstract Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new kawa.lang.WrongArguments(this.name,1,"(?)");
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new kawa.lang.WrongArguments(this.name,1,"(?)");
  }

  public Object apply4 (Object arg1, Object arg2,
			Object arg3, Object arg4)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new kawa.lang.WrongArguments(this.name,1,"(?)");
  }

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length != 1)
      throw new kawa.lang.WrongArguments(this.name,1,"(?)");
    return apply1 (args[0]);
  }
}
