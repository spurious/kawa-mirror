package kawa.standard;
import gnu.math.IntNum;
import gnu.mapping.Procedure1;

/** Implements the Common Lisp function "integer-length". */

public class int_length extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    return IntNum.make (((IntNum)arg1).intLength ());
  }
}
