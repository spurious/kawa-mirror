package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char->integer".
 * @author Per Bothner
 */

public class char2integer extends Procedure1
{
  public char2integer ()
  {
    super ("char->integer");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    return new Integer (((Char)arg1).intValue ());
  }
}
