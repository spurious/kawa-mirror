package kawa.standard;
import kawa.lang.*;

public class car extends Procedure1
{
  public car()
  {
    super("car");
  }

  public Object apply1 (Object arg1)
       throws WrongType
  {
    if (arg1 instanceof Pair)
      return ((Pair)arg1).car;
    else
      throw new WrongType(this.name,1,"list or pair");
  }
}
