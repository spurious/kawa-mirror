package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char-whitespace?".
 * @author Per Bothner
 */

public class char_whitespace_p extends Procedure1
{
  public char_whitespace_p ()
  {
    super ("char-whitespace?");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    char ch = ((Char)arg1).charValue ();
    if (Character.isSpace (ch))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
