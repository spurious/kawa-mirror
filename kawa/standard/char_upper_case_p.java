package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char-upper-case?".
 * @author Per Bothner
 */

public class char_upper_case_p extends Procedure1
{
  public char_upper_case_p ()
  {
    super ("char-upper-case?");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    char ch = ((Char)arg1).charValue ();
    if (Character.isUpperCase (ch))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
