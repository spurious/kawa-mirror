package kawa.standard;

import kawa.lang.Procedure1;

public class real_p extends kawa.lang.Procedure1 {
   public kawa.standard.real_p() {
      super("real?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof java.lang.Double) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
