package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char-lower-case?".
 * @author Per Bothner
 */

public class char_lower_case_p extends Procedure1
{
  public char_lower_case_p ()
  {
    super ("char-lower-case?");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    char ch = ((Char)arg1).charValue ();
    if (Character.isLowerCase (ch))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
