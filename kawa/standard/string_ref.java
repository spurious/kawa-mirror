package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;
import gnu.mapping.Procedure2;
import gnu.mapping.WrongType;

public class string_ref extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
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
