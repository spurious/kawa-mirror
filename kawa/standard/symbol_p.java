package kawa.standard;

import kawa.lang.Procedure1;

public class symbol_p extends kawa.lang.Procedure1 {
   public kawa.standard.symbol_p() {
      super("symbol?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof kawa.lang.symbol) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }
}
