package kawa.standard;

import kawa.lang.Procedure1;

public class integer_p extends kawa.lang.Procedure1 {
   public kawa.standard.integer_p() {
      super("integer?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof java.lang.Integer) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
