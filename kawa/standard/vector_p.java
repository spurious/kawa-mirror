package kawa.standard;

import kawa.lang.Procedure1;

public class vector_p extends kawa.lang.Procedure1 {
   public kawa.standard.vector_p() {
      super("vector?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof kawa.lang.vector) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
