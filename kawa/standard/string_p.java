package kawa.standard;

import kawa.lang.Procedure1;

public class string_p extends kawa.lang.Procedure1 {
   public kawa.standard.string_p() {
      super("string?");
   }

   public Object apply1 (Object arg1)
   {
     if (arg1 instanceof java.lang.StringBuffer) {
        return kawa.lang.Interpreter.trueObject;
     } else {
        return kawa.lang.Interpreter.falseObject;
     }
   }
}
