package kawa.standard;

//-- Exceptions
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

import java.io.PrintStream;

public class make_vector extends kawa.lang.Named implements kawa.lang.Executable {
   public kawa.standard.make_vector() {
      super("make-vector");
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
            Object o = kawa.lang.Interpreter.nullObject;
            if (pair.cdr instanceof kawa.lang.pair) {
               kawa.lang.pair second = (kawa.lang.pair)pair.cdr;
               if (second.cdr instanceof kawa.lang.snull) {
                  o = second.car;
               } else {
                  throw new kawa.lang.WrongArguments(this.name,2,"(make-vector n char)");
               }
            } 

            java.util.Vector v = new java.util.Vector();
            for (int t=0; t<count; t++) {
               v.addElement(o);
            }
            return new kawa.lang.vector(v);
         } else {
            throw new kawa.lang.WrongType(this.name,1,"integer");
         }
      } else {
         throw new kawa.lang.WrongArguments(this.name,1,"(make-vector n obj)");
      }
   }

   public void print(java.io.PrintStream ps) {
      ps.print("#<kawa.standard.make_vector>");
   }
}
