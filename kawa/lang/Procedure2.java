package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public abstract class Procedure2 extends Named implements Executable {

   public Procedure2(java.lang.String n) {
      super(n);
   }

  /*   public abstract Object execute2(kawa.lang.Interpreter i,java.util.Vector frames,Object arg1,Object arg2)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol;
	     */

   public abstract Object apply2 (Object arg1,Object arg2)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol;


   public Object execute(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object list
   ) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (list instanceof kawa.lang.pair) {
         kawa.lang.pair p1 = (kawa.lang.pair)list;
         if (p1.cdr instanceof kawa.lang.pair) {
            kawa.lang.pair p2 = (kawa.lang.pair)p1.cdr;
            if (p2.cdr instanceof kawa.lang.snull) {
               return apply2 (p1.car,p2.car);
            }
         }
      } 
      throw new kawa.lang.WrongArguments(this.name,2,"(?)");
   }
}
