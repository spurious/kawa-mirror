package kawa.standard;
import kawa.lang.*;

import kawa.lang.Named;
import kawa.lang.Executable;

public class or extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.or() {
      super("or");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType
   {
      if (arglist instanceof Pair) {
         boolean b = false;
         int index = 1;
         while (!b && arglist instanceof Pair) {
            Pair pair = (Pair)arglist;
            if (((java.lang.Boolean)pair.car).booleanValue()) {
               b = true;
            }
            index++;
         }
         if (b) {
            return kawa.lang.Interpreter.trueObject;
         } else {
            return kawa.lang.Interpreter.falseObject;
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,1,"(or obj ...)");
      }
   }

}
