package kawa.standard;

import kawa.lang.Procedure2;

public class list_tail extends kawa.lang.Procedure2 {
   public kawa.standard.list_tail() {
      super("list-tail");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) throws kawa.lang.WrongArguments,
            kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof kawa.lang.pair) {
         if (arg2 instanceof java.lang.Integer) {
            kawa.lang.pair list = (kawa.lang.pair)arg1;
            int count = ((java.lang.Integer)arg2).intValue();
            while (count!=0 && list.cdr instanceof kawa.lang.pair) {
               count--;
               list = (kawa.lang.pair)list.cdr;
            }
            if (count!=0) {
               throw new kawa.lang.GenericError("List is too short.");
            }
            return list;
         } else {
            throw new kawa.lang.WrongType(this.name,2,"integer");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"list");
      }
   }
}
