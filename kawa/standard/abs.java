package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class abs extends kawa.lang.Procedure1 {
   public kawa.standard.abs() {
      super("abs");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.Integer) {
         if (((java.lang.Integer)arg1).intValue()<0) {
            arg1 = new java.lang.Integer(((java.lang.Integer)arg1).intValue()*(-1));
         } 
      } else if (arg1 instanceof java.lang.Double) {
         if (((java.lang.Double)arg1).doubleValue()<0.0) {
            arg1 = new java.lang.Double(((java.lang.Double)arg1).doubleValue()*(-1));
         } 
      } else {
         throw new kawa.lang.WrongType(this.name,1,"number");
      }
      return arg1;
   }

}
