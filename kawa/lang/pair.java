package kawa.lang;

import java.io.PrintStream;

public class pair extends Object implements Printable {

   public Object car;
   public Object cdr;

   public kawa.lang.pair(Object carval,Object cdrval) {
      car = carval;
      cdr = cdrval;
   }

   public void print(java.io.PrintStream ps) {
      ps.print("(");
      printNoParen(this, ps);
      ps.print(")");
   }

   static public final void printNoParen(pair p, java.io.PrintStream ps) {
     for (;;) {
       kawa.lang.print.print (p.car, ps);
       Object cdr = p.cdr;
       if (cdr == null || cdr instanceof kawa.lang.snull)
	 break;
       if (cdr instanceof kawa.lang.pair) {
	 ps.print(" ");
	 p = (kawa.lang.pair)cdr;
       } else {
	 ps.print(" . ");
	 kawa.lang.print.print (cdr, ps);
         break;
       }
     }
   }
};
