package kawa.standard;
import kawa.lang.*;

public class cons extends Procedure2
{
  public cons()
  {
    super("cons");
  }

  public Object apply2 (Object arg1, Object arg2) 
  {
    return new Pair (arg1, arg2);
  }
}
