package kawa.standard;

import kawa.lang.Procedure2;

public class cons extends kawa.lang.Procedure2 {
   public kawa.standard.cons() {
      super("cons");
   }

   public Object apply2 (Object arg1, Object arg2) 
   {
      return new kawa.lang.pair(arg1,arg2);
   }
}
