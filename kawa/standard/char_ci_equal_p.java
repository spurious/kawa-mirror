package kawa.standard;
import kawa.lang.*;

/** The char-ci=? procedure
 * @author R. Alexander Milowski
 */

public class char_ci_equal_p extends Procedure2
{
  public char_ci_equal_p()
  {
    super("char-ci=?");
  }
 
  public Object apply2(Object arg1, Object arg2)
       throws WrongType
  {
    char c1 = ((Char)arg1).charValue ();
    char c2 = ((Char)arg2).charValue ();
    if (Character.toUpperCase (c1) == Character.toUpperCase (c2))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}

