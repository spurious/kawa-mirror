package kawa.standard;
import kawa.lang.*;

public class cdddar extends kawa.lang.Procedure1 {
   public kawa.standard.cdddar() {
      super("cdddar");
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
               arg1 = ((Pair)arg1).cdr;          
               if (arg1 instanceof Pair) {
                  return ((Pair)arg1).cdr;
               } else {
                  throw new kawa.lang.GenericError("Result of the cddar not a pair.");
               }
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
