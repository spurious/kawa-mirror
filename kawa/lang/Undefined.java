package kawa.lang;

import java.io.PrintWriter;

public class Undefined extends Object implements Printable {
   public Undefined() {
   }
   public void print(java.io.PrintWriter ps) {
      ps.print("#<undefined>");
   }
}
