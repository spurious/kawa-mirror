package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

/** Implements the Common Lisp function "integer-length". */

public class int_length extends Procedure1
{
  public Object apply1 (Object arg1) throws WrongType
  {
    return IntNum.make (((IntNum)arg1).intLength ());
  }
}
