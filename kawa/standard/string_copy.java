package kawa.standard;

import kawa.lang.Procedure1;

public class string_copy extends kawa.lang.Procedure1 {
   public string_copy() {
      super("string-copy");
   }

   public Object apply1 (Object arg1)
   {
      return new java.lang.StringBuffer(((java.lang.StringBuffer)arg1).toString());
   }
}
