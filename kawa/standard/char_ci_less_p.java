package kawa.standard;
import gnu.kawa.util.Char;
import gnu.mapping.Procedure2;
import gnu.mapping.WrongType;

public class char_ci_less_p extends Procedure2
{
  public Object apply2(Object arg1, Object arg2)
  {
    if (! (arg1 instanceof Char))
      throw new WrongType(this.name(),1,"character");
    if (! (arg2 instanceof Char))
      throw new WrongType(this.name(),2,"character");
    char c1 = ((Char)arg1).charValue ();
    char c2 = ((Char)arg2).charValue ();
    if (Character.toUpperCase (c1) < Character.toUpperCase (c2))
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }
}
