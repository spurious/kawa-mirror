package kawa.lang;

import java.io.PrintStream;

public class snull extends Object implements Printable {
   public snull() {
   }
   public void print(java.io.PrintStream ps) {
      ps.print("()");
   }
}
