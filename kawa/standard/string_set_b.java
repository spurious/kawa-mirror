package kawa.standard;
import kawa.lang.*;
import gnu.math.IntNum;
import gnu.mapping.Procedure3;
import gnu.mapping.WrongType;

public class string_set_b extends Procedure3
{
  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    if (! (arg1 instanceof FString))
      throw new WrongType(this.name(),1,"string");
    FString str = (FString) arg1;
    if (! (arg3 instanceof Char))
      throw new WrongType(this.name(),3,"character");
    try
      {
	str.setCharAt(IntNum.intValue (arg2),
		      ((Char)arg3).charValue());
	return kawa.lang.Interpreter.voidObject;
      }
    catch (StringIndexOutOfBoundsException e)
      {
	throw new GenericError("String index out of bounds.");
      }
  }
}
