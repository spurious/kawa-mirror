package kawa.lang;

public class SetFieldProc extends Procedure2
{
  java.lang.reflect.Field fld;

  SetFieldProc (Class clas, String fname) throws GenericError
  {
    try
      {
	fld = clas.getField(fname);
      }
    catch (NoSuchFieldException ex)
      {
	throw new GenericError ("no such field "+fname+" in "+clas.getName());
      }
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    try
      {
	fld.set(arg1, arg2);
	return Interpreter.voidObject;
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fld.getName());
      }
  }
}
