package kawa.standard;

import kawa.lang.Procedure1;

public class pair_p extends kawa.lang.Procedure1 {
   public kawa.standard.pair_p() {
      super("pair?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof kawa.lang.pair) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
