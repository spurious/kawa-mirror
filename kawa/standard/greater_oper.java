package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Named;
import kawa.lang.Executable;

public class greater_oper extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.greater_oper() {
      super(">");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType
   {
      if (arglist instanceof kawa.lang.pair) {
         kawa.lang.pair pair = (kawa.lang.pair)arglist;

         if (pair.cdr instanceof kawa.lang.pair) {
            boolean retval = true;
            pair = (kawa.lang.pair)pair.car;
            java.lang.Number last = null;
            int count = 1;
            while (retval && pair.cdr instanceof kawa.lang.pair) {
               if (last==null) {
                  if (pair.car instanceof java.lang.Number) {
                     last = (java.lang.Number)pair.car;
                  } else {
                     throw new kawa.lang.WrongType(this.name,count,"number");
                  }
               } else if (pair.car instanceof java.lang.Number) {
                  if (last.doubleValue()<=((java.lang.Number)pair.car).doubleValue()) {
                     retval = false;
                  }
                  last = (java.lang.Number)pair.car;
               } else {
                  throw new kawa.lang.WrongType(this.name,count,"number");
               }
               pair = (kawa.lang.pair)pair.cdr;
               count++;
            }
            if (retval) {
               return i.trueObject;
            } else {
               return i.falseObject;
            }
         } else {
            throw new kawa.lang.WrongArguments(this.name,2,"(> x1 x2 ...)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(> x1 x2 ...)");
      }
   }

}
