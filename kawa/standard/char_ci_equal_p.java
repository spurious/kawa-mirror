package kawa.standard;
import gnu.kawa.util.Char;
import gnu.mapping.*;

/** The char-ci=? procedure
 * @author R. Alexander Milowski
 */

public class char_ci_equal_p extends Procedure2
{
  public Object apply2(Object arg1, Object arg2)
  {
    char c1 = ((Char)arg1).charValue ();
    char c2 = ((Char)arg2).charValue ();
    if (Character.toUpperCase (c1) == Character.toUpperCase (c2))
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }
}

