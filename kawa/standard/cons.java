package kawa.standard;

import kawa.lang.Procedure2;

public class cons extends kawa.lang.Procedure2 {
   public kawa.standard.cons() {
      super("cons");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) 
   {
      return new kawa.lang.pair(arg1,arg2);
   }
}
