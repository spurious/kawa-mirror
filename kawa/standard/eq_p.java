package kawa.standard;

import kawa.lang.Procedure2;

public class eq_p extends kawa.lang.Procedure2 {
   public kawa.standard.eq_p() {
      super("eq?");
   }

   public Object apply2(Object arg1, Object arg2) 
   {
      if (arg1==arg2) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }
}
