package kawa.standard;
import kawa.lang.*;

public class caaddr extends kawa.lang.Procedure1 {
   public kawa.standard.caaddr() {
      super("caaddr");
   }

   public Object apply1 (Object arg1)
     throws kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof Pair) {
         arg1 = ((Pair)arg1).cdr;          
         if (arg1 instanceof Pair) {
            arg1 = ((Pair)arg1).cdr;          
            if (arg1 instanceof Pair) {
               arg1 = ((Pair)arg1).car;          
               if (arg1 instanceof Pair) {
                  return ((Pair)arg1).car;
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
