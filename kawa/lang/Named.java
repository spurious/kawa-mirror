package kawa.lang;

import java.io.PrintStream;
import java.util.Vector;

public class Named extends Object implements Nameable {
   public java.lang.String name;

   public Named(java.lang.String n) {
     name = new java.lang.String(n);
   }

   public java.lang.String name() {
     return name;
   }
}
