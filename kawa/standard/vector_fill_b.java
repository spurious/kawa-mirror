package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure2;

/**
 * Implement the Scheme standard function "vector-fill!".
 */

public class vector_fill_b extends Procedure2
{
  public vector_fill_b()
  {
    super("vector-fill!");
  }

  public Object apply2 (Object arg1,Object arg2)
  {
     kawa.lang.Vector v = (kawa.lang.Vector)arg1;

     int len = v.length();

     for (int t=0; t<len; t++) {
        v.setElementAt(arg2,t);
     }
    return Interpreter.voidObject;
  }
}
