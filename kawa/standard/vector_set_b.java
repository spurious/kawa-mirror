package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class vector_set_b extends Procedure3
{
  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws GenericError
  {
    try
      {
	((Vector)arg1).setElementAt (arg3, IntNum.intValue (arg2));
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
	throw new kawa.lang.GenericError("vector index out of bounds.");
      }
    return Interpreter.voidObject;
  }
}
