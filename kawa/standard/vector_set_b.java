package kawa.standard;
import kawa.lang.*;

public class vector_set_b extends Procedure3
{
  public vector_set_b()
  {
    super("vector-set!");
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws GenericError
  {
    try
      {
	((Vector)arg1).setElementAt (arg3, ((Integer)arg2).intValue ());
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
	throw new kawa.lang.GenericError("vector index out of bounds.");
      }
    return Interpreter.voidObject;
  }
}
