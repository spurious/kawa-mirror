package kawa.lang;

/**
 * Abstract class for 0-argument procedures.
 * @author	Per Bothner
 */

public abstract class Procedure0 extends Procedure
{
  public Procedure0 ()
  {
    super();
  }

  public Procedure0 (java.lang.String n)
  {
    super(n);
  }

  public abstract Object apply0 ()
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
      throw new WrongArguments(this.name (), 0, "(?)");
  }

   public Object apply2 (Object arg1,Object arg2)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
      throw new kawa.lang.WrongArguments(this.name (), 0, "(?)");
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
      throw new kawa.lang.WrongArguments(this.name (), 2, "(?)");
  }

  public Object apply4 (Object arg1, Object arg2,
			Object arg3, Object arg4)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
      throw new kawa.lang.WrongArguments(this.name (), 2, "(?)");
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length != 0)
      throw new kawa.lang.WrongArguments(this.name (), 0, "(?)");
    return apply0 ();
  }
}
