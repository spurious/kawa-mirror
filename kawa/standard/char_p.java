package kawa.standard;

import kawa.lang.Procedure1;
             
public class char_p extends kawa.lang.Procedure1 {
   public kawa.standard.char_p() {
      super("char?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof java.lang.Character) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
