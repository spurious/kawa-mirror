package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

/**
 * Implement the Scheme standard function "char->integer".
 * @author Per Bothner
 */

public class char2integer extends Procedure1
{
  public Object apply1(Object arg1)
       throws WrongType
  {
    return IntNum.make (((Char)arg1).intValue ());
  }
}
