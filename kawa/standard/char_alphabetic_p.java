package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "char-alphabetic?".
 * @author Per Bothner
 */

public class char_alphabetic_p extends Procedure1
{
  public char_alphabetic_p ()
  {
    super ("char-alphabetic?");
  }

  public Object apply1(Object arg1)
       throws WrongType
  {
    char ch = ((Char)arg1).charValue ();
    // Java 1.1 will have Character.isLetter  FIXME use that instead
    if (Character.isLowerCase (ch) || Character.isUpperCase (ch))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
