package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public abstract class Procedure4 extends Named implements Executable {

   public Procedure4(java.lang.String n) {
      super(n);
   }

   public abstract Object apply4(Object arg1,Object arg2,
				 Object arg3,Object arg4)
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
            if (p2.cdr instanceof kawa.lang.pair) {
               kawa.lang.pair p3 = (kawa.lang.pair)p1.cdr;
               if (p3.cdr instanceof kawa.lang.pair) {
                  kawa.lang.pair p4 = (kawa.lang.pair)p1.cdr;
                  if (p4.cdr instanceof kawa.lang.snull) {
                     return apply4(p1.car,p2.car,p3.car,p4.car);
                  }
               }
            }
         }
      } 
      throw new kawa.lang.WrongArguments(this.name,4,"(?)");
   }
}
