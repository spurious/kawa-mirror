package kawa.standard;
import kawa.lang.*;

public class make_promise extends Procedure1
{
  public Object apply1 (Object arg1)
       throws WrongType
  {
    return new Promise ((Procedure0) arg1);
  }
}
