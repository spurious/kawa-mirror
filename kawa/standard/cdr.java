package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class cdr extends kawa.lang.Procedure1 {
   public kawa.standard.cdr() {
      super("cdr");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongType
   {
      if (arg1 instanceof kawa.lang.pair) {
         return ((kawa.lang.pair)arg1).cdr;
      } else {
         throw new kawa.lang.WrongType(this.name,1,"list or pair");
      }
   }

}
