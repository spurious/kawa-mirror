package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char-downcase".
 * @author Per Bothner
 */

public class char_downcase extends Procedure1
{
  public char_downcase ()
  {
    super ("char-downcase");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    char ch = ((Char)arg1).charValue ();
    return Char.make (Character.toLowerCase (ch));
  }
}
