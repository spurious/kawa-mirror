package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "integer->char".
 * @author Per Bothner
 */

public class integer2char extends Procedure1
{
  public integer2char ()
  {
    super ("integer->char");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    int i = ((Integer)arg1).intValue ();
    return Char.make (i);
  }
}
