package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

/**
 * Implement the Scheme standard function "integer->char".
 * @author Per Bothner
 */

public class integer2char extends Procedure1
{
  public Object apply1(Object arg1)
       throws WrongType
  {
    return Char.make (IntNum.intValue (arg1));
  }
}
