package kawa.standard;
import kawa.lang.*;
import gnu.math.*;
import gnu.mapping.*;

public class lognot extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return BitOps.not ((IntNum) arg1);
  }
}
