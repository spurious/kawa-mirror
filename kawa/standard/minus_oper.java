package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;

import kawa.lang.Named;
import kawa.lang.Executable;

public class minus_oper extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.minus_oper() {
      super("-");
   }

   public Object execute(kawa.lang.Interpreter i,java.util.Vector frames,Object arglist) 
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
      if (arglist instanceof kawa.lang.pair) {
         kawa.lang.pair top = (kawa.lang.pair)arglist;
         if (top.cdr instanceof kawa.lang.pair) {
            if (top.car instanceof java.lang.Number) {
               boolean isInteger;
               int ival = 0;
               double dval = 0.0;
               if (top.car instanceof java.lang.Integer) {
                  isInteger = true;
                  ival = ((java.lang.Integer)top.car).intValue();
               } else {
                  isInteger = false;
                  dval = ((java.lang.Double)top.car).doubleValue();
               }
               int count = 2;
               arglist = top.cdr;
               while (arglist instanceof kawa.lang.pair) {
                  top = (kawa.lang.pair)arglist;
                  if (top.car instanceof java.lang.Integer) {
                     if (isInteger) {
                        ival += ((java.lang.Integer)top.car).intValue();
                     } else {
                        dval += ((java.lang.Double)top.car).doubleValue();
                     }
                  } else if (top.car instanceof java.lang.Double) {
                     if (isInteger) {
                        isInteger = false;
                        dval = ival;
                     }
                     dval += ((java.lang.Double)top.car).doubleValue();
                  } else {
                     throw new kawa.lang.WrongType(this.name,count,"number");
                  }
                  arglist = top.cdr;
                  count++;
               }
               if (isInteger) {
                  return new java.lang.Integer(ival);
               } else {
                  return new java.lang.Double(dval);
               }
            } else {
               throw new kawa.lang.WrongType(this.name,1,"number");
            }
         } else {
            if (top.car instanceof java.lang.Double) {
               return new java.lang.Double(0-((java.lang.Double)top.car).intValue());
            } else if (top.car instanceof java.lang.Integer) {
               return new java.lang.Integer(0-((java.lang.Integer)top.car).intValue());
            } else {
               throw new kawa.lang.WrongType(this.name,1,"number");
            }
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,-1,"(- n ...)");
      }
   }

}
