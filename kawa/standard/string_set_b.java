package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;
import kawa.lang.GenericError;

import kawa.lang.Procedure3;

public class string_set_b extends kawa.lang.Procedure3 {
   public kawa.standard.string_set_b() {
      super("string-set!");
   }

   public Object apply3 (Object arg1, Object arg2, Object arg3)
     throws kawa.lang.WrongType,
            kawa.lang.GenericError
   {
      if (arg1 instanceof java.lang.StringBuffer) {
         if (arg2 instanceof java.lang.Integer) {
            if (arg3 instanceof java.lang.Character) {
               try {
                  ((java.lang.StringBuffer)arg1).setCharAt(
                    (int)((java.lang.Integer)arg2).intValue(),
                    ((java.lang.Character)arg3).charValue()
                  );
                  return kawa.lang.Interpreter.undefinedObject;
               } catch (StringIndexOutOfBoundsException e) {
                  throw new kawa.lang.GenericError("String index out of bounds.");
               }
            } else {
               throw new kawa.lang.WrongType(this.name,3,"character");
            }
         } else {
            throw new kawa.lang.WrongType(this.name,2,"integer");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"string");
      }
   }

}
