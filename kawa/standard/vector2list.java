package kawa.standard;
import gnu.kawa.util.*;
import gnu.mapping.Procedure1;

/**
 * Implement the Scheme standard function "vector->list".
 */

public class vector2list extends Procedure1
{
  public Object apply1 (Object arg1)
  {
     FVector v = (FVector)arg1;

     int len = v.length();

     LList result = LList.Empty;
     for (int t=len-1; t>=0; t--) {
        result = new Pair(v.elementAt(t),result);
     }
     return result;
  }
}
