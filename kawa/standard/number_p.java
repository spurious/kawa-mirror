package kawa.standard;

import kawa.lang.Procedure1;

public class number_p extends kawa.lang.Procedure1 {
   public kawa.standard.number_p() {
      super("number?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof java.lang.Number) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
