package kawa.standard;

import kawa.lang.Procedure3;

public class vector_set_b extends kawa.lang.Procedure3 {
   public kawa.standard.vector_set_b() {
      super("vector-set!");
   }

   public Object apply3 (Object arg1, Object arg2, Object arg3)
     throws kawa.lang.GenericError
   {
      try {
         ((kawa.lang.vector)arg1).value.setElementAt(arg3,((java.lang.Integer)arg2).intValue());
         return kawa.lang.Interpreter.undefinedObject;
      } catch (ArrayIndexOutOfBoundsException e) {
         throw new kawa.lang.GenericError("vector index out of bounds.");
      }
   }
}
