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
      if (car instanceof Printable) {
         ((Printable)car).print(ps);
      } else {
         ps.print("#<"+car.getClass().getName()+">");
      }
      if (cdr!=null) {
         if (cdr instanceof kawa.lang.pair) {
            ps.print(" ");
            ((kawa.lang.pair)cdr).printNoParen(ps);
         } else if (cdr instanceof Printable) {
            ps.print(" . ");
            ((Printable)cdr).print(ps);
         } else {
            ps.print(" . ");
            ps.print("#<"+cdr.getClass().getName()+">");
         }
      }
      ps.print(")");
   }

   public void printNoParen(java.io.PrintStream ps) {
      if (car instanceof Printable) {
         ((Printable)car).print(ps);
      } else {
         ps.print("#<"+car.getClass().getName()+">");
      }
      if (cdr!=null && !(cdr instanceof kawa.lang.snull)) {
         if (cdr instanceof kawa.lang.pair) {
            ps.print(" ");
            ((kawa.lang.pair)cdr).printNoParen(ps);
         } else if (cdr instanceof Printable) {
            ps.print(" . ");
            ((Printable)cdr).print(ps);
         } else {
            ps.print(" . ");
            ps.print("#<"+cdr.getClass().getName()+">");
         }
      }
   }
};
