package kawa.standard;
import kawa.lang.*;

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
      if (arglist instanceof Pair) {
         Pair pair = (Pair)arglist;

         if (pair.cdr instanceof Pair) {
            boolean isequal = true;
            java.lang.Number first = null;
            double dval = 0.0;
            int count = 1;
            Object o = pair;
            while (isequal && o instanceof Pair) {
               pair = (Pair)o;
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
               return kawa.lang.Interpreter.trueObject;
            } else {
               return kawa.lang.Interpreter.falseObject;
            }
         } else {
            throw new kawa.lang.WrongArguments(this.name,2,"(= x1 x2 ...)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(= x1 x2 ...)");
      }
   }

}
