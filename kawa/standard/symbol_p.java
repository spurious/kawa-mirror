package kawa.standard;

import kawa.lang.Procedure1;

public class symbol_p extends kawa.lang.Procedure1 {
   public kawa.standard.symbol_p() {
      super("symbol?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof kawa.lang.symbol) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }
}
