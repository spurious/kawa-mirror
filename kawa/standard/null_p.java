package kawa.standard;

import kawa.lang.Procedure1;

public class null_p extends kawa.lang.Procedure1 {
   public kawa.standard.null_p() {
      super("null?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof kawa.lang.snull) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
