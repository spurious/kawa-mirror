package kawa.standard;
import kawa.lang.*;

public class vector_ref extends Procedure2
{
  public vector_ref()
  {
    super("vector-ref");
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError
  {
    try
      {
	return ((Vector)arg1).elementAt (((Integer)arg2).intValue ());
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
	throw new GenericError("vector index out of bounds.");
      }
  }
}
