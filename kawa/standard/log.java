package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "log". */

public class log extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return ((Complex) arg1).log();
  }
}
