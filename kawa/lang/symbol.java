package kawa.lang;

import java.io.PrintStream;

public class symbol extends Object implements Printable {
   public java.lang.String name;
   public symbol(java.lang.String n) {
      name = new java.lang.String(n);
   }
   public void print(java.io.PrintStream ps) {
      ps.print(name);
   }
}
