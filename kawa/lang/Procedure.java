package kawa.lang;

/**
 * The abstract parent for all Scheme functions.
 * @author  Per Bothner
 */

public abstract class Procedure extends Named implements Printable
{
  public Procedure()
  {
    super ();
  }

  public Procedure(java.lang.String n)
  {
    super(n);
  }

  public Procedure(Symbol n)
  {
    super(n);
  }

  public abstract Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

   public abstract Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

   public abstract Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

   public abstract Object apply2 (Object arg1,Object arg2)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

   public abstract Object apply3 (Object arg1, Object arg2, Object arg3)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

   public abstract Object apply4(Object arg1,Object arg2,
				 Object arg3,Object arg4)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<procedure ");
    Symbol n = name ();
    if (n == null)
      ps.print ("<unnamed>");
    else
      ps.print (n);
    ps.print ('>');
  }
}
