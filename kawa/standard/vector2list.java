package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "vector->list".
 */

public class vector2list extends Procedure1
{
  public vector2list()
  {
    super("vector->list");
  }

  public Object apply1 (Object arg1)
       throws WrongType
  {
     kawa.lang.Vector v = (kawa.lang.Vector)arg1;

     int len = v.length();

     kawa.lang.List result = Interpreter.nullObject;
     for (int t=len-1; t>=0; t--) {
        result = new Pair(v.elementAt(t),result);
     }
     return result;
  }
}
