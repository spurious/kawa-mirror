package kawa.standard;

import kawa.lang.Procedure2;

public class equal_p extends kawa.lang.Procedure2 {
   public kawa.standard.equal_p() {
      super("equal?");
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
      } else if (arg1 instanceof java.lang.Boolean &&
                 arg2 instanceof java.lang.Boolean) {
         if (((java.lang.Boolean)arg1).equals((java.lang.Boolean)arg2)) {
            return i.trueObject;
         } 
      } else if (arg1 instanceof kawa.lang.symbol &&
                 arg2 instanceof kawa.lang.symbol) {
         if (((kawa.lang.symbol)arg1).name.equals(((kawa.lang.symbol)arg2).name)) {
            return i.trueObject;
         } 
      } else if (arg1 instanceof java.lang.Double &&
                 arg2 instanceof java.lang.Double) {
         if (((java.lang.Double)arg1).equals((java.lang.Double)arg2)) {
            return i.trueObject;
         } 
      } else if (arg1 instanceof java.lang.Integer &&
                 arg2 instanceof java.lang.Integer) {
         if (((java.lang.Integer)arg1).equals((java.lang.Integer)arg2)) {
            return i.trueObject;
         } 
      } else if (arg1 instanceof java.lang.Character &&
                 arg2 instanceof java.lang.Character) {
         if (((java.lang.Character)arg1).equals((java.lang.Character)arg2)) {
            return i.trueObject;
         } 
      } else if (arg1 instanceof kawa.lang.snull &&
                 arg2 instanceof kawa.lang.snull) {
         return i.trueObject;
      } 
      return i.falseObject;
   }

}
