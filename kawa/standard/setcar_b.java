package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure2;

public class setcar_b extends kawa.lang.Procedure2 {
   public kawa.standard.setcar_b() {
      super("set-car!");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) throws kawa.lang.WrongType 
   {
      if (arg1 instanceof kawa.lang.pair) {
         ((kawa.lang.pair)arg1).car = arg2;
         return i.undefinedObject;
      } else {
         throw new kawa.lang.WrongType(this.name,1,"pair");
      }
   }

}
