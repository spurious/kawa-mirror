package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure2;

public class string_ref extends kawa.lang.Procedure2 {
   public kawa.standard.string_ref() {
      super("string-ref");
   }

   public Object apply2 (Object arg1, Object arg2) throws kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof java.lang.StringBuffer) {
         if (arg2 instanceof java.lang.Integer) {
            try {
               return new java.lang.Character(((java.lang.StringBuffer)arg1).charAt(((java.lang.Integer)arg2).intValue()));
            } catch (StringIndexOutOfBoundsException e) {
               throw new kawa.lang.GenericError("String index out of bounds.");
            }
         } else {
            throw new kawa.lang.WrongType(this.name,2,"integer");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"string");
      }
   }

}
