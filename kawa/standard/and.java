package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import kawa.lang.Named;
import kawa.lang.Executable;

public class and extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.and() {
      super("and");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType
   {
      if (arglist instanceof kawa.lang.pair) {
         boolean b = true;
         int index = 1;
         while (b && arglist instanceof kawa.lang.pair) {
            kawa.lang.pair pair = (kawa.lang.pair)arglist;
            if (!((java.lang.Boolean)pair.car).booleanValue()) {
               b = false;
            }
            index++;
         }
         if (b) {
            return i.trueObject;
         } else {
            return i.falseObject;
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,1,"(and obj ...)");
      }
   }

}
