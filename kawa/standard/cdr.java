package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure1;
import gnu.mapping.HasSetter;
import gnu.mapping.WrongType;

public class cdr extends Procedure1 implements HasSetter
{
  public static Object apply (Pair arg)
  {
    return arg.cdr;
  }

  public Object apply1 (Object arg1)
  {
    try
      {
        return ((Pair)arg1).cdr;
      }
    catch (ClassCastException ex)
      {
        throw WrongType.make(ex, this, 0);
      }
  }

  public void set1 (Object value, Object pair)
  {
    ((Pair) pair).cdr = value;
  }
}
