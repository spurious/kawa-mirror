package kawa.standard;

import kawa.lang.Procedure0;
import kawa.lang.Exit;

public class exit extends kawa.lang.Procedure0 {
   public kawa.standard.exit() {
      super("exit");
   }

   public Object apply0 ()
   {
      return new kawa.lang.Exit();
   }

   public void print(java.io.PrintStream ps) {
      ps.print("#<kawa.standard.exit>");
   }
}
