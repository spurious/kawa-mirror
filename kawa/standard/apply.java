package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import kawa.lang.Procedure2;

public class apply extends kawa.lang.Named {
   public kawa.standard.apply() {
      super("apply");
   }

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
               return i.apply(p1.car,p2.car, frames);
            }
         }
      } 
      throw new kawa.lang.WrongArguments(this.name,2,"(?)");
   }

   public void print(java.io.PrintStream ps) {
      ps.print("#<kawa.standard.apply>");
   }
}
