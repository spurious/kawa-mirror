package kawa.lang;

/**
 * The abstract parent for all Scheme functions.
 * @author  Per Bothner
 */

public abstract class Procedure extends Named implements Executable 
{
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
     if (list instanceof kawa.lang.snull)
       return apply0 ();

     kawa.lang.pair p1 = (kawa.lang.pair)list;
     Object arg1 = p1.car;
     if (p1.cdr instanceof kawa.lang.snull)
       return apply1 (arg1);
     kawa.lang.pair p2 = (kawa.lang.pair)p1.cdr;
     Object arg2 = p2.car;
     if (p2.cdr instanceof kawa.lang.snull)
       return apply2 (arg1, arg2);
     kawa.lang.pair p3 = (kawa.lang.pair)p2.cdr;
     Object arg3 = p3.car;
     if (p3.cdr instanceof kawa.lang.snull)
       return apply3 (arg1, arg2, arg3);
     kawa.lang.pair p4 = (kawa.lang.pair)p3.cdr;
     Object arg4 = p4.car;
     if (p4.cdr instanceof kawa.lang.snull)
       return apply4 (arg1, arg2, arg3, arg4);
     int count = kawa.standard.length.length (p4);
     Object[] args = new Object [count + 4];
     args[0] = arg1;
     args[1] = arg2;
     args[2] = arg3;
     args[3] = arg4;
     list = p4.cdr;
     for (int i = 0; i < count; i++) {
       kawa.lang.pair pair = (kawa.lang.pair) list;
       args[i+4] = pair.car;
       list = pair.cdr;
     }
     return applyN (args);
   }
}
