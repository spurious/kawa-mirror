package kawa.standard;
import kawa.lang.*;

public class multiply_oper extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.multiply_oper() {
      super("*");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
      int ival = 1;
      double dval = 0.0;
      boolean isInteger = true;
      int count = 1;
      while (arglist instanceof Pair) {
         Pair pair = (Pair)arglist;
         if (pair.car instanceof java.lang.Integer) {
            if (isInteger) {
               ival = ival * ((java.lang.Integer)pair.car).intValue();
            } else {
               dval = dval * ((java.lang.Integer)pair.car).intValue();
            }
         } else if (pair.car instanceof java.lang.Double) {
            if (isInteger) {
               isInteger = false;
               dval = ival;
            }
            dval = dval * ((java.lang.Double)pair.car).doubleValue();
         } else {
            throw new kawa.lang.WrongType(this.name,count,"number");
         }
         arglist = pair.cdr;
         count++;
      }

      if (isInteger) {
        return new java.lang.Integer(ival);
      } else {
        return new java.lang.Double(dval);
      }

   }

}
