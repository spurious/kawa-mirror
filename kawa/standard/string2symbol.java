package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class string2symbol extends kawa.lang.Procedure1 {
   public kawa.standard.string2symbol() {
      super("string->symbol");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.StringBuffer) {
         return new kawa.lang.symbol(((java.lang.StringBuffer)arg1).toString().toLowerCase());
      } else {
         throw new kawa.lang.WrongType(this.name,1,"string");
      }
   }

}
