package kawa.standard;

import kawa.lang.Procedure1;

public class number_p extends kawa.lang.Procedure1 {
   public kawa.standard.number_p() {
      super("number?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof java.lang.Number) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
