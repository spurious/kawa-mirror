package kawa.standard;
import kawa.lang.*;

public class make_future extends Procedure1
{
  public Object apply1 (Object arg1)
       throws WrongType
  {
    Future f = new Future ((Procedure0) arg1);
    f.start();
    return f;
  }
}
