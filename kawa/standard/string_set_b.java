package kawa.standard;
import kawa.lang.*;

public class string_set_b extends Procedure3
{
  public string_set_b()
  {
    super("string-set!");
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3)
       throws WrongType, GenericError
  {
    if (! (arg1 instanceof StringBuffer))
      throw new WrongType(this.name,1,"string");
    StringBuffer str = (StringBuffer) arg1;
    if (! (arg2 instanceof Integer))
      throw new WrongType(this.name,2,"integer");
    if (! (arg3 instanceof Char))
      throw new WrongType(this.name,3,"character");
    try
      {
	str.setCharAt((int)((Integer)arg2).intValue(),
		      ((Char)arg3).charValue());
	return kawa.lang.Interpreter.voidObject;
      }
    catch (StringIndexOutOfBoundsException e)
      {
	throw new GenericError("String index out of bounds.");
      }
  }
}
