package kawa.standard;

import kawa.lang.Procedure1;
             
public class char_p extends kawa.lang.Procedure1 {
   public kawa.standard.char_p() {
      super("char?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof java.lang.Character) {
         return kawa.lang.Interpreter.trueObject;
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
