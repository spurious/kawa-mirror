package kawa.standard;

import kawa.lang.Procedure1;

public class null_p extends kawa.lang.Procedure1 {
   public kawa.standard.null_p() {
      super("null?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof kawa.lang.snull) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
