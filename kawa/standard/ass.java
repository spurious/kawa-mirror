package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure2;

public class ass extends kawa.lang.Procedure2 {
   protected kawa.lang.Procedure2 compare;
   protected java.lang.String usage;
   public kawa.standard.ass(java.lang.String name,kawa.lang.Procedure2 comp) {
      super(name);
      compare = comp;
      usage = new java.lang.String("("+name+" obj list)");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) throws kawa.lang.WrongType,
            kawa.lang.WrongArguments,
            kawa.lang.GenericError,
            kawa.lang.UnboundSymbol
   {
      if (arg2 instanceof kawa.lang.pair) {
         kawa.lang.pair list = (kawa.lang.pair)arg2;

         if (!(list.car instanceof kawa.lang.pair)) {
            throw new kawa.lang.GenericError("The association list contains non-pair elements.");
         }

         do {
            java.lang.Boolean check = (java.lang.Boolean)compare.execute2(i,frames,((kawa.lang.pair)list.car).car,arg1);

            if (check.booleanValue()) {
               return list.car;
            } 

            if (list.cdr instanceof kawa.lang.pair) {
               list = (kawa.lang.pair)list.cdr;
               if (!(list.car instanceof kawa.lang.pair)) {
                  throw new kawa.lang.GenericError("The association list contains non-pair elements.");
               }
            } else {
               list = null;
            }

         } while (list!=null);

         return i.falseObject;
      } else if (arg2 instanceof kawa.lang.snull) {
         return i.falseObject;
      } else {
         throw new kawa.lang.WrongType(this.name,2,"list");
      }
   }
}
