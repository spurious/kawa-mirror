package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class positive_p extends kawa.lang.Procedure1 {
   public kawa.standard.positive_p() {
      super("positive?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.Double) {
         if (((java.lang.Double)arg1).doubleValue()>0.0) {
            return i.trueObject;
         } else {
            return i.falseObject;
         }
      } else if (arg1 instanceof java.lang.Integer) {
         if (((java.lang.Integer)arg1).intValue()>0) {
            return i.trueObject;
         } else {
            return i.falseObject;
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"number");
      }
   }

}
