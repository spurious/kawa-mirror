package kawa.standard;
import kawa.lang.*;

public class string_ref extends kawa.lang.Procedure2
{
  public string_ref()
  {
    super("string-ref");
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongType, GenericError
  {
    if (! (arg1 instanceof StringBuffer))
      throw new WrongType(this.name,1,"string");
    if (! (arg2 instanceof Integer))
      throw new WrongType(this.name,2,"integer");
    try
      {
	return Char.make (((StringBuffer)arg1).charAt(((Integer)arg2).intValue()));
      }
    catch (StringIndexOutOfBoundsException e)
      {
	throw new GenericError("String index out of bounds.");
      }
  }
}
