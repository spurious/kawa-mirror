package kawa.lang;

/**
 * Abstract class for 3-argument Scheme procedures..
 * @author	Per Bothner
 */

public abstract class Procedure3 extends Procedure
{
  public Procedure3 ()
  {
    super();
  }

  public Procedure3(java.lang.String n)
  {
    super(n);
  }

  public Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,3,"(?)");
  }

  public Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,3,"(?)");
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,3,"(?)");
  }

  public abstract Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    throw new WrongArguments(this.name,2,"(?)");
  }

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length != 3)
      throw new WrongArguments(this.name,3,"(?)");
    return apply3 (args[0], args[1], args[2]);
  }
}
