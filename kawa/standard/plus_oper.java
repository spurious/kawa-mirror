package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Named;
import kawa.lang.Executable;

public class plus_oper extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.plus_oper() {
      super("+");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
      int ival = 0;
      double dval = 0.0;
      boolean isInteger = true;
      int count = 1;
      while (arglist instanceof kawa.lang.pair) {
         kawa.lang.pair pair = (kawa.lang.pair)arglist;
         if (pair.car instanceof java.lang.Double) {
            if (isInteger) {
               isInteger = false;
               dval = ival;
            }
            dval += ((java.lang.Double)pair.car).doubleValue();
         } else if (pair.car instanceof java.lang.Integer) {
            if (isInteger) {
               ival += ((java.lang.Integer)pair.car).intValue();
            } else {
               dval += ((java.lang.Integer)pair.car).intValue();
            }
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
