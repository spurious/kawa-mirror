package kawa.lang;

import java.io.PrintStream;

public class Undefined extends Object implements Printable {
   public Undefined() {
   }
   public void print(java.io.PrintStream ps) {
      ps.print("#<undefined>");
   }
}
