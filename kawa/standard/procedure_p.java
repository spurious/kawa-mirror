package kawa.standard;

import kawa.lang.Procedure1;

public class procedure_p extends kawa.lang.Procedure1 {
   public kawa.standard.procedure_p() {
      super("procedure?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof kawa.lang.Executable) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
