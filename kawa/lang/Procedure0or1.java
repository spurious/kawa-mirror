package kawa.lang;

/**
 * Abstract class for 0- or 1-argument Scheme procedures.
 * Extensions must provide apply0 and apply1.
 * @author	Per Bothner
 */

public abstract class Procedure0or1 extends Procedure
{

  public Procedure0or1 ()
  {
    super();
  }

  public Procedure0or1 (String n)
  {
    super(n);
  }

  public abstract Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public abstract Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,0,"(? [arg1])");
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,0,"(? [arg1])");
  }

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,0,"(? [arg1])");
  }

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length == 0)
      return apply0 ();
    else if (args.length == 1)
      return apply1 (args[0]);
    else
      throw new WrongArguments(this.name,0,"(? [arg1])");
  }
}
