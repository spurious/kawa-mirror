package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure2;
import java.io.PrintStream;

public class vector_ref extends kawa.lang.Procedure2 {
   public kawa.standard.vector_ref() {
      super("vector-ref");
   }

   public Object apply2 (Object arg1, Object arg2)
     throws kawa.lang.WrongArguments,
            kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      try {
         return ((kawa.lang.vector)arg1).value.elementAt((int)((java.lang.Integer)arg1).intValue());
      } catch (ArrayIndexOutOfBoundsException e) {
         throw new kawa.lang.GenericError("vector index out of bounds.");
      }
   }

}
