package gnu.expr;
import gnu.mapping.*;

import java.io.PrintWriter;

public class Undefined extends Object implements Printable {
  static private Undefined undef = null;
  static public Undefined getInstance()
  {
    if (undef == null)
      undef = new Undefined();
    return undef;
  }

   public Undefined() {
   }
   public void print(java.io.PrintWriter ps) {
      ps.print("#<undefined>");
   }
}
