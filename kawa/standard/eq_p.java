package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme function "eq?". */

public class eq_p extends Procedure2 {

  public Object apply2(Object arg1, Object arg2) 
  {
    if (arg1==arg2)
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }
}
