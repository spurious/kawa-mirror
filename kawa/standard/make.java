package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

public class make extends ProcedureN
{
  public int numArgs() { return 0xFFFFF001; } // minimum 1 argument

  public Object applyN (Object[] args)
  {
    int nargs = args.length;
    if (nargs == 0)
      throw new WrongArguments(this, nargs);
    Object arg_0 = args[0];
    Class clas;
    if (arg_0 instanceof Class)
      clas = (Class) arg_0;
    else if (arg_0 instanceof gnu.bytecode.ClassType)
      clas = ((gnu.bytecode.ClassType) arg_0).getReflectClass();
    else
      clas = null;
    if (clas == null)
      throw new WrongType("make", 1, "class");
    Object result;
    try
      {
	result = clas.newInstance();
      }
    catch (InstantiationException ex)
      {
	throw new GenericError (ex.toString());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError (ex.toString());
      }
    for (int i = 1;  i < nargs;  )
      {
	Keyword key = (Keyword) args[i++];
	Object arg = args[i++];
	Record.set1(result, arg, key.getName());
      }
    return result;
  }
}
