package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class car extends kawa.lang.Procedure1 {
   public kawa.standard.car() {
      super("car");
   }

   public Object apply1 (Object arg1)
     throws kawa.lang.WrongType
   {
      if (arg1 instanceof kawa.lang.pair) {
         return ((kawa.lang.pair)arg1).car;
      } else {
         throw new kawa.lang.WrongType(this.name,1,"list or pair");
      }
   }

}
