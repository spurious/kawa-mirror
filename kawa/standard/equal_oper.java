package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Executable;
import java.io.PrintStream;

public class equal_oper extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.equal_oper() {
      super("=");
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

         if (pair.cdr instanceof kawa.lang.pair) {
            boolean isequal = true;
            java.lang.Number first = null;
            double dval = 0.0;
            int count = 1;
            Object o = pair;
            while (isequal && o instanceof kawa.lang.pair) {
               pair = (kawa.lang.pair)o;
               if (first==null) {
                  if (pair.car instanceof java.lang.Number) {
                     first = (java.lang.Number)pair.car;
                     dval = first.doubleValue();
                  } else {
                     throw new kawa.lang.WrongType(this.name,count,"number");
                  }
               } else if (pair.car instanceof java.lang.Number) {
                  if (!(dval==((java.lang.Number)pair.car).doubleValue())) {
                     isequal = false;
                  }
               } else {
                  throw new kawa.lang.WrongType(this.name,count,"number");
               }
               o = pair.cdr;
               count++;
            }
            if (isequal) {
               return i.trueObject;
            } else {
               return i.falseObject;
            }
         } else {
            throw new kawa.lang.WrongArguments(this.name,2,"(= x1 x2 ...)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(= x1 x2 ...)");
      }
   }

}
