package kawa.standard;
import kawa.lang.*;

public class car extends Procedure1 implements HasSetter
{
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
