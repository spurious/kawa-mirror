package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;
import kawa.lang.GenericError;

import kawa.lang.Procedure2;

public class list_ref extends kawa.lang.Procedure2 {
   protected kawa.lang.Procedure2 list_tail;
   public kawa.standard.list_ref(kawa.lang.Procedure2 proc) {
      super("list-ref");
      list_tail = proc;
   }

   public Object apply2 (Object arg1, Object arg2)
     throws kawa.lang.WrongArguments,
            kawa.lang.WrongType,
            kawa.lang.GenericError,
            kawa.lang.UnboundSymbol
   {
      Object tail = list_tail.apply2 (arg1, arg2);

      if (tail instanceof kawa.lang.pair) {
         return ((kawa.lang.pair)tail).car;
      } else {
         throw new kawa.lang.GenericError("List is too short.");
      }
   }
}
