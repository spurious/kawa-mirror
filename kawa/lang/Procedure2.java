package kawa.lang;

/**
 * Abstract class for 2-argument Scheme procedures.
 * Extensions must provide apply2.
 * @author	Per Bothner
 */

public abstract class Procedure2 extends Procedure
{

  public Procedure2(java.lang.String n)
  {
    super(n);
  }
  public Procedure2()
  {
    super();
  }

  public Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,2,"(?)");
  }

  public Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,2,"(?)");
  }

  public abstract Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,2,"(?)");
  }

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,2,"(?)");
  }

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length != 2)
      throw new WrongArguments(this.name,2,"(?)");
    return apply2 (args[0], args[1]);
  }
}
