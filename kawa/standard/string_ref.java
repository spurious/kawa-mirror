package kawa.standard;
import kawa.lang.*;
import kawa.math.IntNum;

public class string_ref extends kawa.lang.Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
       throws WrongType, GenericError
  {
    if (! (arg1 instanceof FString))
      throw new WrongType(this.name(),1,"string");
    try
      {
	return Char.make (((FString)arg1).charAt(IntNum.intValue (arg2)));
      }
    catch (StringIndexOutOfBoundsException e)
      {
	throw new GenericError("String index out of bounds.");
      }
  }
}
