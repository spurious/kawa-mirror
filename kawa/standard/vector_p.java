package kawa.standard;

import kawa.lang.Procedure1;

public class vector_p extends kawa.lang.Procedure1 {
   public kawa.standard.vector_p() {
      super("vector?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof kawa.lang.vector) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
