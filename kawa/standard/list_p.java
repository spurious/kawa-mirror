package kawa.standard;

import kawa.lang.Procedure1;

public class list_p extends kawa.lang.Procedure1 {
   public kawa.standard.list_p() {
      super("list?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) 
   {
      if (arg1 instanceof kawa.lang.pair) {
         kawa.lang.pair p = (kawa.lang.pair)arg1;
         while (p.cdr instanceof kawa.lang.pair) {
            p = (kawa.lang.pair)p.cdr;
         }
         if (p.cdr instanceof kawa.lang.snull) {
            return i.trueObject;
         } else {
            return i.falseObject;
         }
      } else {
         return i.falseObject;
      }
   }

}
