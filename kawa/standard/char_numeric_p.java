package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char-numeric?".
 * @author Per Bothner
 */

public class char_numeric_p extends Procedure1
{
  public char_numeric_p ()
  {
    super ("char-numeric?");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    char ch = ((Char)arg1).charValue ();
    if (Character.isDigit (ch))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
