package kawa.standard;
import kawa.lang.*;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class cddar extends kawa.lang.Procedure1 {
   public kawa.standard.cddar() {
      super("cddar");
   }

   public Object apply1 (Object arg1)
     throws kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof Pair) {
         arg1 = ((Pair)arg1).car;          
         if (arg1 instanceof Pair) {
            arg1 = ((Pair)arg1).cdr;          
            if (arg1 instanceof Pair) {
                  return ((Pair)arg1).cdr;
            } else {
               throw new kawa.lang.GenericError("Result of the cdar not a pair.");
            }
         } else {
            throw new kawa.lang.GenericError("Result of the car not a pair.");
         }
      } else {
         throw new kawa.lang.WrongType(this.name(),1,"list");
      }
   }

}
