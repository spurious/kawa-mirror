package kawa.standard;

import kawa.lang.Procedure1;

public class list_p extends kawa.lang.Procedure1 {
   public kawa.standard.list_p() {
      super("list?");
   }

   public Object apply1 (Object arg1)
   {
      if (arg1 instanceof kawa.lang.pair) {
         kawa.lang.pair p = (kawa.lang.pair)arg1;
         while (p.cdr instanceof kawa.lang.pair) {
            p = (kawa.lang.pair)p.cdr;
         }
         if (p.cdr instanceof kawa.lang.snull) {
            return kawa.lang.Interpreter.trueObject;
         } else {
            return kawa.lang.Interpreter.falseObject;
         }
      } else {
         return kawa.lang.Interpreter.falseObject;
      }
   }

}
