package kawa.standard;

import kawa.lang.Procedure1;

public class pair_p extends kawa.lang.Procedure1 {
   public kawa.standard.pair_p() {
      super("pair?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof kawa.lang.pair) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
