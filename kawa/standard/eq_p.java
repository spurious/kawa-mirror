package kawa.standard;

import kawa.lang.Procedure2;

public class eq_p extends kawa.lang.Procedure2 {
   public kawa.standard.eq_p() {
      super("eq?");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) 
   {
      if (arg1==arg2) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }
}
