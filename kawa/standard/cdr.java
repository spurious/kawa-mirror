package kawa.standard;
import kawa.lang.*;

public class cdr extends Procedure1 implements HasSetter
{
  public Object apply1 (Object arg1)
       throws WrongType
  {
    if (arg1 instanceof Pair)
      return ((Pair)arg1).cdr;
    else
      throw new WrongType(this.name (), 1, "pair");
  }

  public void set1 (Object value, Object pair)
  {
    ((Pair) pair).cdr = value;
  }
}
