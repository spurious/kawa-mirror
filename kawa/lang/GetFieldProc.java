package kawa.lang;

public class GetFieldProc extends Procedure1
{
  java.lang.reflect.Field fld;

  GetFieldProc (Class clas, String fname) throws GenericError
  {
    try
      {
	fld = clas.getField (fname);
      }
    catch (NoSuchFieldException ex)
      {
	throw new GenericError ("no such field "+fname+" in "+clas.getName());
      }
  }

  public Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	return fld.get(arg1);
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fld.getName());
      }
  }
}
