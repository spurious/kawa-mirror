package kawa.standard;

//-- Exceptions
import kawa.lang.GenericError;
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class cdadar extends kawa.lang.Procedure1 {
   public kawa.standard.cdadar() {
      super("cdadar");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof kawa.lang.pair) {
         arg1 = ((kawa.lang.pair)arg1).car;          
         if (arg1 instanceof kawa.lang.pair) {
            arg1 = ((kawa.lang.pair)arg1).cdr;          
            if (arg1 instanceof kawa.lang.pair) {
               arg1 = ((kawa.lang.pair)arg1).car;          
               if (arg1 instanceof kawa.lang.pair) {
                  return ((kawa.lang.pair)arg1).cdr;
               } else {
                  throw new kawa.lang.GenericError("Result of the cadar not a pair.");
               }
            } else {
               throw new kawa.lang.GenericError("Result of the cdar not a pair.");
            }
         } else {
            throw new kawa.lang.GenericError("Result of the car not a pair.");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"list");
      }
   }

}
