package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class vector_ref extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError
  {
    try
      {
	return ((Vector)arg1).elementAt (IntNum.intValue (arg2));
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
	throw new GenericError("vector index out of bounds.");
      }
  }
}
