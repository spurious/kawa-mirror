package kawa.standard;

//-- Exceptions
import kawa.lang.GenericError;
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class caaddr extends kawa.lang.Procedure1 {
   public kawa.standard.caaddr() {
      super("caaddr");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof kawa.lang.pair) {
         arg1 = ((kawa.lang.pair)arg1).cdr;          
         if (arg1 instanceof kawa.lang.pair) {
            arg1 = ((kawa.lang.pair)arg1).cdr;          
            if (arg1 instanceof kawa.lang.pair) {
               arg1 = ((kawa.lang.pair)arg1).car;          
               if (arg1 instanceof kawa.lang.pair) {
                  return ((kawa.lang.pair)arg1).car;
               } else {
                  throw new kawa.lang.GenericError("Result of the caddr not a pair.");
               }
            } else {
               throw new kawa.lang.GenericError("Result of the cddr not a pair.");
            }
         } else {
            throw new kawa.lang.GenericError("Result of the cdr not a pair.");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"list");
      }
   }

}
