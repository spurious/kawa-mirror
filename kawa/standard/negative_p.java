package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class negative_p extends kawa.lang.Procedure1 {
   public kawa.standard.negative_p() {
      super("negative?");
   }

   public Object apply1 (Object arg1)
     throws kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.Double) {
         if (((java.lang.Double)arg1).doubleValue()<0.0) {
            return kawa.lang.Interpreter.trueObject;
         } else {
            return kawa.lang.Interpreter.falseObject;
         }
      } else if (arg1 instanceof java.lang.Integer) {
         if (((java.lang.Integer)arg1).intValue()<0) {
            return kawa.lang.Interpreter.trueObject;
         } else {
            return kawa.lang.Interpreter.falseObject;
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"number");
      }
   }

}
