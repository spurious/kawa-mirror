package kawa.standard;
import gnu.kawa.util.Char;
import gnu.mapping.*;

public class char_equal_p extends Procedure2
{
  public Object apply2(Object arg1, Object arg2)
  {
    if (! (arg1 instanceof Char))
      throw new WrongType(this.name (), 1, "character");
    if (! (arg2 instanceof Char))
      throw new WrongType(this.name (), 2, "character");
    if (((Char)arg1).intValue () == ((Char)arg2).intValue())
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }
}
