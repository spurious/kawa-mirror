package kawa.lang;

import kawa.lang.Printable;
import java.io.PrintStream;

public class Exit extends Object implements Printable {
   public kawa.lang.Exit() {
   }
   public void print(java.io.PrintStream ps) {
      ps.print("#<kawa.lang.Exit>");
   }
}
