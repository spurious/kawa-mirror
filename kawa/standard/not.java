package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import kawa.lang.Procedure1;
import java.io.PrintStream;

public class not extends kawa.lang.Procedure1 {
   public kawa.standard.not() {
      super("not");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongArguments,
            kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.Boolean) {
         java.lang.Boolean b = (java.lang.Boolean)arg1;
         if (b.booleanValue()) {
            return i.falseObject;
         } else {
            return i.trueObject;
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"boolean");
      }
   }

}
