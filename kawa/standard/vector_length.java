package kawa.standard;

import kawa.lang.Procedure1;

public class vector_length extends kawa.lang.Procedure1 {
   public kawa.standard.vector_length() {
      super("vector-length");
   }

   public Object apply1 (Object arg1)
     throws kawa.lang.WrongType
   {
      if (arg1 instanceof kawa.lang.vector) {
         return new java.lang.Integer(((kawa.lang.vector)arg1).size);
      } else {
         throw new kawa.lang.WrongType(this.name,1,"vector");
      }
   }

}
