package kawa.standard;
import kawa.lang.*;
import gnu.math.*;

/** Implement the standard Scheme procedure "exp". */

public class exp extends Procedure1
{
  public Object apply1 (Object arg1) throws WrongType
  {
    return ((Complex) arg1).exp();
  }
}
