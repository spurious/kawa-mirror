package kawa.standard;

import kawa.lang.Procedure2;

public class string_greaterthanequal_p extends kawa.lang.Procedure2 {
   public string_greaterthanequal_p() {
      super("string>=?");
   }

   public Object apply2 (Object arg1, Object arg2)
   {
      if (((java.lang.StringBuffer)arg1).toString().compareTo(
             ((java.lang.StringBuffer)arg2).toString()
          )>=0) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
