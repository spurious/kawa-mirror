package kawa.standard;
import kawa.lang.*;

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
      return new java.lang.Integer(((Vector)arg1).length ());
    else
      throw new kawa.lang.WrongType(this.name(),1,"vector");
  }
}
