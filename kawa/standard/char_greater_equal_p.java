package kawa.standard;

//-- Exceptions
import kawa.lang.WrongType;

import kawa.lang.Procedure2;

public class char_greater_equal_p extends kawa.lang.Procedure2 {
   public kawa.standard.char_greater_equal_p() {
      super("char>=?");
   }

   public Object execute2(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arg1,
      Object arg2
   ) throws kawa.lang.WrongType
   {
      if (arg1 instanceof java.lang.Character) {
         if (arg2 instanceof java.lang.Character) {
            if (((java.lang.Character)arg1).charValue()>=
                ((java.lang.Character)arg2).charValue()) {
               return i.trueObject;
            } else {
               return i.falseObject;
            }
         } else {
            throw new kawa.lang.WrongType(this.name,2,"character");
         }
      } else {
         throw new kawa.lang.WrongType(this.name,1,"character");
      }
   }
}
