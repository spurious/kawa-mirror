package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char-upcase".
 * @author Per Bothner
 */

public class char_upcase extends Procedure1
{
  public char_upcase ()
  {
    super ("char-upcase");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    char ch = ((Char)arg1).charValue ();
    return Char.make (Character.toUpperCase (ch));
  }
}
