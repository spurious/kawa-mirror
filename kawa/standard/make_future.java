package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

public class make_future extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    Future f = new Future((Procedure) arg1);
    f.start();
    return f;
  }
}
