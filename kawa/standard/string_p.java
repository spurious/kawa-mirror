package kawa.standard;

import kawa.lang.Procedure1;

public class string_p extends kawa.lang.Procedure1 {
   public kawa.standard.string_p() {
      super("string?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
     if (arg1 instanceof java.lang.StringBuffer) {
        return i.trueObject;
     } else {
        return i.falseObject;
     }
   }
}
