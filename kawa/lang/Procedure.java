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

  public Procedure(String n)
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

  public final int minArgs() { return numArgs() & 0xFFF; }
  public final int maxArgs() { return numArgs() >> 12; }

  /** Check that the number of arguments in a call is valid.
    * @param proc the Procedure being called
    * @param argCount the number of arguments in the call
    * @return nothing, if the number of arguments is ok
    * @exception kawa.lang.WrongArguments there are too many or too
    *     few actual arguments
    */
  public static void checkArgCount(Procedure proc, int argCount)
  {
    int num = proc.numArgs();
    if (argCount < (num & 0xFFF)
	|| (num >= 0 && argCount > (num >> 12)))
      throw new WrongArguments(proc, argCount);
  }

  /** Return minArgs()|(maxArgs<<12). */

  /* We use a single virtual function to reduce the number of methods
   * in the system, as well as the number of virtual method table entries.
   * We shift by 12 so the number can normally be represented using a
   * sipush instruction, with requiring a constant pool entry.
   */
  public int numArgs() { return 0xfffff000; }

  public Procedure getSetter()
  {
    if (! (this instanceof HasSetter))
      throw new GenericError("procedure "+name()+ "has no setter");
    int num_args = numArgs();
    if (num_args == 0x0000)
      return new Setter0(this);
    if (num_args == 0x1001)
      return new Setter1(this);
    return new Setter(this);
  }

  /** If HasSetter, the Procedure is called in the LHS of an assignment. */
  public void set0(Object result)
  {
    getSetter().apply1(result);
  }

  public void set1(Object result, Object arg1)
  {
    getSetter().apply2(result, arg1);
  }

  public void setN (Object[] args)
  {
    getSetter().applyN(args);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<procedure ");
    String n = name ();
    if (n == null)
      ps.print ("<unnamed>");
    else
      ps.print (n);
    ps.print ('>');
  }
}
