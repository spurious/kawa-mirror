package kawa.standard;

import kawa.lang.Procedure1;

public class integer_p extends kawa.lang.Procedure1 {
   public kawa.standard.integer_p() {
      super("integer?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof java.lang.Integer) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
