package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure1;
import gnu.mapping.HasSetter;
import gnu.mapping.WrongType;

public class car extends Procedure1 implements HasSetter
{
  public static Object apply (Pair arg)
  {
    return arg.car;
  }

  public Object apply1 (Object arg1)
  {
    if (arg1 instanceof Pair)
      return ((Pair)arg1).car;
    else
      throw new WrongType(this.name (), 1, "pair");
  }

  public void set1 (Object value, Object pair)
  {
    ((Pair) pair).car = value;
  }
}
