package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class vector_length extends Procedure1
{
  public vector_length()
  {
    super("vector-length");
  }

  public Object apply1 (Object arg1)
       throws kawa.lang.WrongType
  {
    if (arg1 instanceof Vector)
      return IntNum.make (((Vector)arg1).length ());
    else
      throw new kawa.lang.WrongType(this.name(),1,"vector");
  }
}
