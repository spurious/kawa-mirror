package gnu.expr;
import gnu.mapping.*;

import java.io.PrintWriter;

public class Undefined extends Object implements Printable {
  public static final Undefined undefined = new Undefined();
  static public Undefined getInstance()
  {
    return undefined;
  }

   public Undefined() {
   }
   public void print(java.io.PrintWriter ps) {
      ps.print("#<undefined>");
   }
}
