package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import kawa.lang.Named;
import kawa.lang.Executable;

public class string extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.string() {
      super("string");
   }

   public Object execute(
      kawa.lang.Interpreter i,
      java.util.Vector frames,
      Object arglist
   ) throws kawa.lang.WrongArguments,
            kawa.lang.WrongType
   {
      int count = 0;
      Object o = arglist;
      while (o instanceof kawa.lang.pair) {
         count++;
         if (((kawa.lang.pair)o).car instanceof java.lang.Character) {
            o = ((kawa.lang.pair)o).cdr;
         } else {
            throw new kawa.lang.WrongType(this.name,count,"character");
         }
      }

      //-- TODO: Hack... allocated two times.
      char value[] = new char[count];
      int index = 0;

      o = arglist;
      while (o instanceof kawa.lang.pair) {
         count++;
         value[index] = ((java.lang.Character)((kawa.lang.pair)o).car).charValue();
         index++;
         o = ((kawa.lang.pair)o).cdr;
      }

      StringBuffer foo = new java.lang.StringBuffer();
      foo.append(value);
      return foo;

   }

}
