package kawa.standard;
import kawa.lang.*;

public class greater_oper extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.greater_oper() {
      super(">");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType
   {
      if (arglist instanceof Pair) {
         Pair pair = (Pair)arglist;

         if (pair.cdr instanceof Pair) {
            boolean retval = true;
            pair = (Pair)pair.car;
            java.lang.Number last = null;
            int count = 1;
            while (retval && pair.cdr instanceof Pair) {
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
               pair = (Pair)pair.cdr;
               count++;
            }
            if (retval) {
               return kawa.lang.Interpreter.trueObject;
            } else {
               return kawa.lang.Interpreter.falseObject;
            }
         } else {
            throw new kawa.lang.WrongArguments(this.name,2,"(> x1 x2 ...)");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,2,"(> x1 x2 ...)");
      }
   }

}
