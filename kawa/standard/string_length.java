package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure1;

public class string_length extends kawa.lang.Procedure1 {
   public kawa.standard.string_length() {
      super("string-length");
   }

   public Object apply1 (Object arg1)
     throws kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.StringBuffer) {
         return new java.lang.Integer(((java.lang.StringBuffer)arg1).length());
      } else {
         throw new kawa.lang.WrongType(this.name(),1,"string");
      }
   }

}
