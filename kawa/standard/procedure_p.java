package kawa.standard;

import kawa.lang.Procedure1;

public class procedure_p extends kawa.lang.Procedure1 {
   public kawa.standard.procedure_p() {
      super("procedure?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof kawa.lang.Executable) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
