package kawa.standard;

import kawa.lang.Procedure1;

public class real_p extends kawa.lang.Procedure1 {
   public kawa.standard.real_p() {
      super("real?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof java.lang.Double) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
