package kawa.lang;

/**
 * Abstract class for 4-argument Scheme procedures.
 * @author	Per Bothner
 */

public abstract class Procedure4 extends Procedure
{

  public Procedure4(java.lang.String n)
  {
    super(n);
  }

  public Object apply0 ()
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new kawa.lang.WrongArguments(this.name,4,"(?)");
  }

  public Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,4,"(?)");
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,4,"(?)");
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,4,"(?)");
  }

  public abstract Object apply4(Object arg1,Object arg2,
				Object arg3,Object arg4)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length != 4)
      throw new WrongArguments(this.name,4,"(?)");
    return apply4 (args[0], args[1], args[2], args[3]);
  }
}
