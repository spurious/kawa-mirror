package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import kawa.lang.Executable;
import kawa.lang.Named;

public class make_string extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.make_string() {
      super("make-string");
   }

   public Object execute(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arglist
   ) throws kawa.lang.WrongArguments,
            kawa.lang.WrongType
   {
      if (arglist instanceof kawa.lang.pair) {
         kawa.lang.pair pair = (kawa.lang.pair)arglist;

         if (pair.car instanceof java.lang.Integer) {
            int count = ((java.lang.Integer)pair.car).intValue();
            char c = 0;
            if (pair.cdr instanceof kawa.lang.pair) {
               kawa.lang.pair second = (kawa.lang.pair)pair.cdr;
               if (second.cdr instanceof kawa.lang.snull) {
                  if (second.car instanceof java.lang.Character) {
                     c = ((java.lang.Character)second.car).charValue();
                  } else {
                     throw new kawa.lang.WrongArguments(this.name,2,"character");
                  }
               } else {
                  throw new kawa.lang.WrongArguments(this.name,1,"(make-string n char)");
               }
            } 

            //-- TODO: complete hack... allocated two times
            char value[] = new char[count];

            for (int t=0; t<count; t++) {
               value[t] = c;
            }
            StringBuffer foo = new java.lang.StringBuffer();
            foo.append(value);
            return foo;
         } else {
            throw new kawa.lang.WrongArguments(this.name,1,"(make-string n char)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,1,"(make-string n char)");
      }
   }

}
