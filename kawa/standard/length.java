package kawa.standard;

import kawa.lang.Procedure1;

public class length extends kawa.lang.Procedure1 {
   public kawa.standard.length() {
      super("length");
   }

   public Object apply1 (Object arg1)
   {
      int count = 0;
      if (arg1 instanceof kawa.lang.pair) {
         kawa.lang.pair p = (kawa.lang.pair)arg1;
         while (p.cdr instanceof kawa.lang.pair) {
            p = (kawa.lang.pair)p.cdr;
            count++;
         }
      } 
      return new java.lang.Integer(count);
   }

}
