package kawa.standard;

//-- Exceptions
import kawa.lang.GenericError;
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class caaadr extends kawa.lang.Procedure1 {
   public kawa.standard.caaadr() {
      super("caaadr");
   }

   public Object apply1 (Object arg1)
     throws kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof kawa.lang.pair) {
         arg1 = ((kawa.lang.pair)arg1).cdr;          
         if (arg1 instanceof kawa.lang.pair) {
            arg1 = ((kawa.lang.pair)arg1).car;          
            if (arg1 instanceof kawa.lang.pair) {
               arg1 = ((kawa.lang.pair)arg1).car;          
               if (arg1 instanceof kawa.lang.pair) {
                  return ((kawa.lang.pair)arg1).car;
               } else {
                  throw new kawa.lang.GenericError("Result of the caadr not a pair.");
               }
            } else {
               throw new kawa.lang.GenericError("Result of the cadr not a pair.");
            }
         } else {
            throw new kawa.lang.GenericError("Result of the cdr not a pair.");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"list");
      }
   }

}
