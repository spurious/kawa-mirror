package kawa.lang;

/**
 * The abstract parent for all Scheme functions.
 * @author  Per Bothner
 */

public abstract class Procedure extends Named implements Executable, Printable
{
  public Procedure()
  {
    super("<unknown>");
  }

  public Procedure(java.lang.String n)
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

   // Obsolete (soon)
   public Object execute(Interpreter interp, java.util.Vector frames,
			 Object list) 
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
   {
     if (list == List.Empty)
       return apply0 ();

     Pair p1 = (Pair)list;
     Object arg1 = p1.car;
     if (p1.cdr == List.Empty)
       return apply1 (arg1);
     Pair p2 = (Pair)p1.cdr;
     Object arg2 = p2.car;
     if (p2.cdr == List.Empty)
       return apply2 (arg1, arg2);
     Pair p3 = (Pair)p2.cdr;
     Object arg3 = p3.car;
     if (p3.cdr == List.Empty)
       return apply3 (arg1, arg2, arg3);
     Pair p4 = (Pair)p3.cdr;
     Object arg4 = p4.car;
     if (p4.cdr == List.Empty)
       return apply4 (arg1, arg2, arg3, arg4);
     int count = kawa.standard.length.length (p4);
     Object[] args = new Object [count + 4];
     args[0] = arg1;
     args[1] = arg2;
     args[2] = arg3;
     args[3] = arg4;
     list = p4.cdr;
     for (int i = 0; i < count; i++) {
       Pair pair = (Pair) list;
       args[i+4] = pair.car;
       list = pair.cdr;
     }
     return applyN (args);
   }

  public void print(java.io.PrintStream ps)
  {
    ps.print ("#<procedure ");
    String n = name ();
    ps.print (n == null ? "<null>" : n);
    ps.print ('>');
  }
}
