package kawa.standard;
import kawa.lang.*;

public class divide_oper extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.divide_oper() {
      super("/");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
      double dval = 0.0;
      int count = 1;
      while (arglist instanceof Pair) {
         Pair pair = (Pair)arglist;
         if (pair.car instanceof java.lang.Number) {
            if (count==1) {
               dval = ((java.lang.Number)pair.car).doubleValue();
            } else {
               dval = dval / ((java.lang.Number)pair.car).doubleValue();
            }
         } else {
            throw new kawa.lang.WrongType(this.name,count,"number");
         }
         arglist = pair.cdr;
         count++;
      }

      if (count==1) {
         dval = 1/dval;
      }

      return new java.lang.Double(dval);

   }

}
