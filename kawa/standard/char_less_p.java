package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure2;
import gnu.mapping.WrongType;

public class char_less_p extends Procedure2
{
  public Object apply2(Object arg1, Object arg2)
  {
    if (! (arg1 instanceof Char))
      throw new WrongType(this.name(),1,"character");
    if (! (arg2 instanceof Char))
      throw new WrongType(this.name(),2,"character");
    if (((Char)arg1).intValue () < ((Char)arg2).intValue())
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }
}
