package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Procedure1;
import java.io.PrintStream;

public class boolean_p extends kawa.lang.Procedure1 {
   public kawa.standard.boolean_p() {
      super("boolean?");
   }

   public Object execute1(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1
   ) throws kawa.lang.WrongArguments 
   {
      if (arg1 instanceof java.lang.Boolean) {
         return i.trueObject;
      } else {
         return i.falseObject;
      }
   }

}
