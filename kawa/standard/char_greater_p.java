package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure2;

public class char_greater_p extends kawa.lang.Procedure2 {
   public kawa.standard.char_greater_p() {
      super("char>?");
   }

   public Object apply2 (Object arg1, Object arg2)
       throws kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.Character) {
         if (arg2 instanceof java.lang.Character) {
            if (((java.lang.Character)arg1).charValue()>
                ((java.lang.Character)arg2).charValue()) {
               return kawa.lang.Interpreter.trueObject;
            } else {
               return kawa.lang.Interpreter.falseObject;
            }
         } else {
            throw new kawa.lang.WrongType(this.name,2,"character");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"character");
      }
   }

}
