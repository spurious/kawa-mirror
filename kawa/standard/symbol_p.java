package kawa.standard;
import kawa.lang.*;

public class symbol_p extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return Scheme.boolObject (arg1 instanceof String);
  }
}
