package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class symbol2string extends kawa.lang.Procedure1 {
   public kawa.standard.symbol2string() {
      super("symbol->string");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongType
   {
      if (arg1 instanceof kawa.lang.symbol) {
         return new java.lang.StringBuffer(((kawa.lang.symbol)arg1).name);
      } else {
         throw new kawa.lang.WrongType(this.name,1,"symbol");
      }
   }

}
